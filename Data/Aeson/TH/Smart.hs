{-# LANGUAGE CPP, NoImplicitPrelude, TemplateHaskell, OverloadedStrings, ScopedTypeVariables #-}
-- Shamelessly copied from Bryan O'Sullivan, 2011

module Data.Aeson.TH.Smart
    ( deriveJSON

    , deriveToJSON
    , deriveFromJSON

    , mkToJSON
    , mkParseJSON
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from aeson:
import Data.Aeson ( toJSON, Object, object, (.=), (.:?)
                  , ToJSON, toJSON
                  , FromJSON, parseJSON
                  )
import Data.Aeson.Types ( Value(..), Parser )
-- from base:
import Control.Applicative ( pure, (<$>), (<*>) )
import Control.Monad       ( return, mapM, liftM2, fail )
import Data.Bool           ( otherwise)
import Data.Default        ( def, Default )
import Data.Eq             ( (==) )
import Data.Function       ( ($), (.), id )
import Data.Functor        ( fmap )
import Data.List           ( (++), foldl, foldl', intercalate
                           , length, map, zip, genericLength
                           )
import Data.Maybe          ( Maybe(Nothing, Just) )
import Prelude             ( String, (-), Integer, fromIntegral, not,
                             error, filter, fst, snd, Bool(..), flip, maybe, (>))
import Text.Printf         ( printf )
import Text.Show           ( show )
#if __GLASGOW_HASKELL__ < 700
import Control.Monad       ( (>>=) )
import Prelude             ( fromInteger )
#endif
-- from unordered-containers:
import qualified Data.HashMap.Strict as H ( lookup, toList, size )
-- from template-haskell:
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
-- from text:
import qualified Data.Text as T ( Text, pack, unpack )
-- from vector:
import qualified Data.Vector as V ( unsafeIndex, null, length, create, filter)
import qualified Data.Vector.Mutable as VM ( unsafeNew, unsafeWrite )

--------------------------------------------------------------------------------
-- Convenience
--------------------------------------------------------------------------------

-- | Generates both 'ToJSON' and 'FromJSON' instance declarations for the given
-- data type.
--
-- This is a convienience function which is equivalent to calling both
-- 'deriveToJSON' and 'deriveFromJSON'.
deriveJSON :: (String -> String)
           -- ^ Function to change field names.
           -> Name
           -- ^ Name of the type for which to generate 'ToJSON' and 'FromJSON'
           -- instances.
           -> Q [Dec]
deriveJSON withField name =
    liftM2 (++)
           (deriveToJSON   withField name)
           (deriveFromJSON withField name)


--------------------------------------------------------------------------------
-- ToJSON
--------------------------------------------------------------------------------

{-
TODO: Don't constrain phantom type variables.

data Foo a = Foo Int
instance (ToJSON a) ⇒ ToJSON Foo where ...

The above (ToJSON a) constraint is not necessary and perhaps undesirable.
-}

-- | Generates a 'ToJSON' instance declaration for the given data type.
--
-- Example:
--
-- @
-- data Foo = Foo 'Char' 'Int'
-- $('deriveToJSON' 'id' ''Foo)
-- @
--
-- This will splice in the following code:
--
-- @
-- instance 'ToJSON' Foo where
--      'toJSON' =
--          \value -> case value of
--                      Foo arg1 arg2 -> 'Array' $ 'V.create' $ do
--                        mv <- 'VM.unsafeNew' 2
--                        'VM.unsafeWrite' mv 0 ('toJSON' arg1)
--                        'VM.unsafeWrite' mv 1 ('toJSON' arg2)
--                        return mv
-- @
deriveToJSON :: (String -> String)
             -- ^ Function to change field names.
             -> Name
             -- ^ Name of the type for which to generate a 'ToJSON' instance
             -- declaration.
             -> Q [Dec]
deriveToJSON withField name =
    withType name $ \tvbs cons -> fmap (:[]) $ fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs cons =
        instanceD (return $ map (\t -> ClassP ''ToJSON [VarT t]) typeNames)
                  (classType `appT` instanceType)
                  [ funD 'toJSON
                         [ clause []
                                  (normalB $ consToJSON withField cons)
                                  []
                         ]
                  ]
      where
        classType = conT ''ToJSON
        typeNames = map tvbName tvbs
        instanceType = foldl' appT (conT name) $ map varT typeNames

-- | Generates a lambda expression which encodes the given data type as JSON.
--
-- Example:
--
-- @
-- data Foo = Foo Int
-- @
--
-- @
-- encodeFoo :: Foo -> 'Value'
-- encodeFoo = $('mkToJSON' id ''Foo)
-- @
--
-- This will splice in the following code:
--
-- @
-- \value -> case value of Foo arg1 -> 'toJSON' arg1
-- @
mkToJSON :: (String -> String) -- ^ Function to change field names.
         -> Name -- ^ Name of the type to encode.
         -> Q Exp
mkToJSON withField name = withType name (\_ cons -> consToJSON withField cons)

-- | Helper function used by both 'deriveToJSON' and 'mkToJSON'. Generates code
-- to generate the JSON encoding of a number of constructors. All constructors
-- must be from the same type.
consToJSON :: (String -> String)
           -- ^ Function to change field names.
           -> [Con]
           -- ^ Constructors for which to generate JSON generating code.
           -> Q Exp
consToJSON _ [] = error $ "Data.Aeson.TH.consToJSON: "
                          ++ "Not a single constructor given!"
-- A single constructor is directly encoded. The constructor itself may be
-- forgotten.
consToJSON withField [con] = do
    value <- newName "value"
    lam1E (varP value)
          $ caseE (varE value)
                  [encodeArgs Nothing withField con]

consToJSON withField cons = do
	    value <- newName "value"
	    lam1E (varP value)
	          $ caseE (varE value)
	                  [ encodeArgs (Just $ wrap $ [|String . T.pack|] `appE` conNameExp con) withField con
	                  | con <- cons
	                  ]
  where
    wrap :: Q Exp -> [Q Exp] -> Q Exp
    wrap name exps =
        [e|object|] `appE` ([e| filter (not .(==Null) . snd )|] `appE`
            listE (infixApp (litE $ stringL "constructor") [e|(.=)|] name : exps))

-- | Generates code to generate the JSON encoding of a single constructor.
encodeArgs :: Maybe ([Q Exp] -> Q Exp) -> (String -> String) -> Con -> Q Match
encodeArgs _ _ c@(NormalC conName []) =
    match (conP conName []) (normalB $ [e|toJSON|] `appE` ([|T.pack|] `appE` conNameExp c)) []
encodeArgs wrapper _ (NormalC conName ts) = do
    let len = length ts
    args <- mapM newName ["arg" ++ show n | n <- [1..len]]
    let js = case [[e|toJSON|] `appE` varE arg | arg <- args] of
              -- Single argument is directly converted.
              [e] -> e
              -- Multiple arguments are converted to a JSON array.
              es  -> do
                 mv <- newName "mv"
                 let newMV = bindS (varP mv)
                                  ([e|VM.unsafeNew|] `appE`
                                    litE (integerL $ fromIntegral len))
                     stmts = [noBindS $
                                [e|VM.unsafeWrite|] `appE`
                                  (varE mv) `appE`
                                    litE (integerL ix) `appE` e | (ix, e) <- zip [(0::Integer)..] es]
                     ret = noBindS $ [e|return|] `appE` varE mv
                     fltr = [e| V.filter (not . (== Null))|]
                 [e|\x-> if V.length x > 0 then Array x else Null|] `appE` (fltr `appE` (varE 'V.create `appE` doE (newMV:stmts++[ret])))
    let b = case wrapper of
              Nothing -> js
              (Just wrapper') -> wrapper' [infixApp (litE (stringL "value")) [e|(.=)|] js]
    match (conP conName $ map varP args) (normalB b) []
-- Records.
encodeArgs withExp withField (RecC conName ts) = do
    args <- mapM newName ["arg" ++ show n | (_, n) <- zip ts [1 :: Integer ..]]
    let args' = map (([e|toJSON|] `appE`) . varE) args
    let js = [ infixApp ([e|T.pack|] `appE` fieldNameExp withField field) [e|(.=)|] arg
             | (arg, (field, _, _)) <- zip args' ts
             ]
    let b = case withExp of
              Nothing -> [e|object|] `appE` ([e| filter (not . disposable . snd) |] `appE` listE js)
              (Just wrapper) -> wrapper js
    match (conP conName $ map varP args) (normalB b) []
-- Infix constructors.
encodeArgs withExp _ (InfixC _ conName _) = do
    al <- newName "argL"
    ar <- newName "argR"
    let l = listE [[e|toJSON|] `appE` varE a | a <- [al,ar]]
    let b = case withExp of
              Nothing -> [e|toJSON|] `appE` l
              (Just wrapper) -> wrapper [infixApp (litE $ stringL "value") [e|(.=)|] l]
    match (infixP (varP al) conName (varP ar)) (normalB b) []
-- Existentially quantified constructors.
encodeArgs withExp withField (ForallC _ _ con) =
    encodeArgs withExp withField con

disposable Null = True
disposable (Array x) = V.null x
disposable _ = False

--------------------------------------------------------------------------------
-- FromJSON
--------------------------------------------------------------------------------

-- | Generates a 'FromJSON' instance declaration for the given data type.
--
-- Example:
--
-- @
-- data Foo = Foo Char Int
-- $('deriveFromJSON' id ''Foo)
-- @
--
-- This will splice in the following code:
--
-- @
-- instance 'FromJSON' Foo where
--     'parseJSON' =
--         \value -> case value of
--                     'Array' arr ->
--                       if (V.length arr == 2)
--                       then Foo \<$\> 'parseJSON' (arr `V.unsafeIndex` 0)
--                                \<*\> 'parseJSON' (arr `V.unsafeIndex` 1)
--                       else fail \"\<error message\>\"
--                     other -> fail \"\<error message\>\"
-- @
deriveFromJSON :: (String -> String)
               -- ^ Function to change field names.
               -> Name
               -- ^ Name of the type for which to generate a 'FromJSON' instance
               -- declaration.
               -> Q [Dec]
deriveFromJSON withField name =
    withType name $ \tvbs cons -> fmap (:[]) $ fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs cons =
        instanceD (return $ map (\t -> ClassP ''FromJSON [VarT t]) typeNames)
                  (classType `appT` instanceType)
                  [ funD 'parseJSON
                         [ clause []
                                  (normalB $ consFromJSON name withField cons)
                                  []
                         ]
                  ]
      where
        classType = conT ''FromJSON
        typeNames = map tvbName tvbs
        instanceType = foldl' appT (conT name) $ map varT typeNames

-- | Generates a lambda expression which parses the JSON encoding of the given
-- data type.
--
-- Example:
--
-- @
-- data Foo = Foo 'Int'
-- @
--
-- @
-- parseFoo :: 'Value' -> 'Parser' Foo
-- parseFoo = $('mkParseJSON' id ''Foo)
-- @
--
-- This will splice in the following code:
--
-- @
-- \\value -> case value of arg -> Foo \<$\> 'parseJSON' arg
-- @
mkParseJSON :: (String -> String) -- ^ Function to change field names.
            -> Name -- ^ Name of the encoded type.
            -> Q Exp
mkParseJSON withField name =
    withType name (\_ cons -> consFromJSON name withField cons)

-- if it's 1ary flat constrcutor, it's just the constructor name, no matter how many
-- if there's many nary constructors, we make an object with value and constructor records
-- if there's many record constructors, we add a record with the constructor value

-- | Helper function used by both 'deriveFromJSON' and 'mkParseJSON'. Generates
-- code to parse the JSON encoding of a number of constructors. All constructors
-- must be from the same type.
consFromJSON :: Name
             -- ^ Name of the type to which the constructors belong.
             -> (String -> String)
             -- ^ Function to change field names.
             -> [Con]
             -- ^ Constructors for which to generate JSON parsing code.
             -> Q Exp
consFromJSON _ _ [] = error $ "Data.Aeson.TH.consFromJSON: "
                              ++ "Not a single constructor given!"

consFromJSON tName withField cons = do
    obj <- newName "obj"
    strcon <- newName "strcon"
    val <- newName "val"
    matcher <- newName "matcher"
    mcon <- newName "mcon"
    arg  <- newName "arg"
    lam1E (varP arg) $ doE [
      bindS (tupP [varP mcon, varP matcher]) $ caseE (varE arg) [
        flip (match (conP 'Object [varP obj])) [] $ normalB $ doE [
          bindS (varP strcon) (sigE ([e|(.:? "constructor")|] `appE` (varE obj)) [t|Parser (Maybe T.Text)|])
          , bindS (varP val) ([e|(.:? "value")|] `appE` (varE obj))
          , noBindS ([|return|] `appE` tupE [varE strcon, [|flip maybe id|] `appE` varE arg `appE` varE val])]
        , match wildP (normalB $ [|return|] `appE` tupE [conE 'Nothing, varE arg]) []]
      , noBindS $ caseE (varE matcher) ([parseCon tName withField c (varE mcon) | c <- cons] ++ [noMatch tName])]


conEq :: ExpQ -> Name -> ExpQ
conEq str conName = infixApp str [|(==)|] ([|T.pack|] `appE` (litE $ stringL $ nameBase conName))

tupSeq :: (Q a, Q b) -> Q (a, b)
tupSeq (a,b) = do
  a' <- a
  b' <- b
  return (a', b')

conGuard :: ExpQ -> Name -> Q Guard
conGuard mcon conName = do
  a <- newName "a" 
  normalG $
    caseE mcon [
      match (conP 'Just [varP a]) (normalB $ conEq (varE a) conName) [],
      match wildP (normalB [|True|]) []]

parseCon :: Name -> (String -> String) -> Con -> ExpQ -> Q Match
parseCon _ _ (NormalC conName []) _ = do
  str <- newName "str"
  grd <- normalG $ conEq (varE str) conName
  expr <- conE conName
  match (conP 'String [varP str]) (guardedB $ [tupSeq (normalG $ conEq (varE str) conName, [|return|] `appE` conE conName)]) []
parseCon _ _ (NormalC conName [_]) mcon = do
  arg <- newName "arg"
  match (varP arg) (guardedB [tupSeq (conGuard mcon conName, 
    infixApp (conE conName) [e|(<$>)|] ([e|parseJSON|] `appE` varE arg))]) []
parseCon tName _  (NormalC conName ts) mcon = parseProduct tName conName (genericLength ts) mcon
parseCon tName withField (RecC conName ts) mcon = do
    obj <- newName "recObj"
    let (x:xs) = [do
                    b <- isInstance ''Default [ty]
                    [|lookupField|]
                      `appE` (if b then [| Just def |] else [| Nothing|])
                      `appE` (litE $ stringL $ show tName)
                      `appE` (litE $ stringL $ nameBase conName)
                      `appE` (varE obj)
                      `appE` ([e|T.pack|] `appE` fieldNameExp withField field)
                  | (field, _, ty) <- ts]
    match (conP 'Object [varP obj])
      (guardedB [tupSeq (conGuard mcon conName, foldl' (\a b -> infixApp a [|(<*>)|] b)
                    (infixApp (conE conName) [|(<$>)|] x) xs)]) []
parseCon tName _ (InfixC _ conName _) mcon = parseProduct tName conName 2 mcon
parseCon tName withField (ForallC _ _ con) mcon = parseCon tName withField con mcon


-- | Generates code to parse the JSON encoding of an n-ary
-- constructor.
parseProduct :: Name -- ^ Name of the type to which the constructor belongs.
             -> Name -- ^ 'Con'structor name.
             -> Integer -- ^ 'Con'structor arity.
             -> ExpQ -- ^ Possible requirement of the constructor
             -> Q Match
parseProduct tName conName numArgs mcon = do
  arr <- newName "arr"
  let x:xs = [[|parseJSON|] `appE`
                    infixApp (varE arr) [|V.unsafeIndex|] (litE $ integerL ix)
                    | ix <- [0 .. numArgs - 1]]
  flip (match (conP 'Array [varP arr])) [] $
    guardedB [tupSeq (
      conGuard mcon conName,
      condE (infixApp ([|V.length|] `appE` varE arr) [|(==)|] (litE $ integerL numArgs))
          (foldl' (\a b -> infixApp a [|(<*>)|] b)
              (infixApp (conE conName) [|(<$>)|] x) xs)
          (parseTypeMismatch tName conName
              (litE $ stringL $ "Array of length " ++ show numArgs)
                                (infixApp (litE $ stringL $ "Array of length ")
                                    [|(++)|] ([|show . V.length|] `appE` varE arr))))]

lookupField :: (FromJSON a) => Maybe a -> String -> String -> Object -> T.Text -> Parser a
lookupField d tName rec obj key =
    case H.lookup key obj of
      Nothing -> case d of
        Nothing -> unknownFieldFail tName rec (T.unpack key)
        Just x -> return x
      Just v  -> parseJSON v

--------------------------------------------------------------------------------
-- Parsing errors
--------------------------------------------------------------------------------

noMatch :: Name -> MatchQ
noMatch tName = do
  flip (match wildP) []
    (normalB $ [| fail $ printf "No constructors for type %s were present." |]
      `appE` (sigE (litE $ stringL $ nameBase tName) (conT ''String)))

parseTypeMismatch :: Name -> Name -> ExpQ -> ExpQ -> ExpQ
parseTypeMismatch tName conName expected actual =
    foldl appE
          [|parseTypeMismatch'|]
          [ litE $ stringL $ nameBase conName
          , litE $ stringL $ show tName
          , expected
          , actual
          ]

unknownFieldFail :: String -> String -> String -> Parser fail
unknownFieldFail tName rec key =
    fail $ printf "When parsing the record %s of type %s the key %s was not present."
                  rec tName key

parseTypeMismatch' :: String -> String -> String -> String -> Parser fail
parseTypeMismatch' tName conName expected actual =
    fail $ printf "When parsing the constructor %s of type %s expected %s but got %s."
                  conName tName expected actual

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- | Boilerplate for top level splices.
--
-- The given 'Name' must be from a type constructor. Furthermore, the
-- type constructor must be either a data type or a newtype. Any other
-- value will result in an exception.
withType :: Name
         -> ([TyVarBndr] -> [Con] -> Q a)
         -- ^ Function that generates the actual code. Will be applied
         -- to the type variable binders and constructors extracted
         -- from the given 'Name'.
         -> Q a
         -- ^ Resulting value in the 'Q'uasi monad.
withType name f = do
    info <- reify name
    case info of
      TyConI dec ->
        case dec of
          DataD    _ _ tvbs cons _ -> f tvbs cons
          NewtypeD _ _ tvbs con  _ -> f tvbs [con]
          other -> error $ "Data.Aeson.TH.withType: Unsupported type: "
                          ++ show other
      _ -> error "Data.Aeson.TH.withType: I need the name of a type."

-- | Extracts the name from a constructor.
getConName :: Con -> Name
getConName (NormalC name _)  = name
getConName (RecC name _)     = name
getConName (InfixC _ name _) = name
getConName (ForallC _ _ con) = getConName con

guardConName :: Name -> Name -> Q Stmt
guardConName conName varName = noBindS (infixApp (litE $ stringL $ nameBase conName) [e|(==)|] (varE varName))

-- | Extracts the name from a type variable binder.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name  ) = name
tvbName (KindedTV name _) = name

-- | Makes a string literal expression from a constructor's name.
conNameExp :: Con -> Q Exp
conNameExp = litE . stringL . nameBase . getConName

-- | Creates a string literal expression from a record field name.
fieldNameExp :: (String -> String) -- ^ Function to change the field name.
             -> Name
             -> Q Exp
fieldNameExp f = litE . stringL . f . nameBase

-- | The name of the outermost 'Value' constructor.
valueConName :: Value -> String
valueConName (Object _) = "Object"
valueConName (Array  _) = "Array"
valueConName (String _) = "String"
valueConName (Number _) = "Number"
valueConName (Bool   _) = "Boolean"
valueConName Null       = "Null"
