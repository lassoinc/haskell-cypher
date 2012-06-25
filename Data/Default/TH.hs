{-# LANGUAGE TemplateHaskell #-}
module Data.Default.TH (deriveDefault) where

import Control.Applicative
import Data.Default
import Data.List
import Language.Haskell.TH

createInstance :: Name -> [Name] -> Name -> [Type] -> Q Dec
createInstance typeConstructorName typeVariables constructorName constructorArgumentTypes = do
	let reqs = constraints typeVariables
	return $ InstanceD reqs
		(AppT (ConT ''Default) (foldl' (\x y -> AppT x (VarT y)) (ConT typeConstructorName) typeVariables))
		[FunD 'def [Clause [] (NormalB (foldl' (\x _ -> AppE x (VarE 'def)) (ConE constructorName) constructorArgumentTypes)) []]]

constraints :: [Name] -> [Pred]
constraints = map (ClassP ''Default . return . VarT)

instanceQ :: Name -> [TyVarBndr] -> Name -> [Type] -> Q [Dec]
instanceQ t vs c as = return <$> createInstance t (map name vs) c as

name :: TyVarBndr -> Name
name (PlainTV n) = n
name (KindedTV n k) = n

deriveDefault :: Name -> Q [Dec]
deriveDefault n = do
	info <- reify n
	case info of
		TyConI (DataD _ qn tvars (con:_) _) -> case con of
			NormalC  conName ts -> instanceQ qn tvars conName (map snd ts)
			RecC     conName ts -> instanceQ qn tvars conName (map (\(v,s,t) -> t) ts)
			InfixC t conName t' -> instanceQ qn tvars conName (map snd [t, t'])
			_ -> fail $ "Dunno how to derive Default instances for existential types"
		TyConI (DataD _ _ _ [] _) -> fail $ "Really? You want to derive a Default instance for an uninhabited type?"
		_ -> fail $ "Couldn't derive a Default instance; didn't know what to do with " ++ pprint info
