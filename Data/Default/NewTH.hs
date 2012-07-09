{-# LANGUAGE TemplateHaskell #-}
module Data.Default.NewTH (deriveDefault) where

import Control.Applicative
import Data.Default
import Data.List
import Language.Haskell.TH

createInstance :: Bool -> Name -> [Name] -> Name -> [Type] -> Q Dec
createInstance b typeConstructorName typeVariables constructorName constructorArgumentTypes = do
	return $ InstanceD (if b then constraints typeVariables else [])
		(AppT (ConT ''Default) (foldl' (\x y -> AppT x (VarT y)) (ConT typeConstructorName) typeVariables))
		[FunD 'def [Clause [] (NormalB (foldl' (\x _ -> AppE x (VarE 'def)) (ConE constructorName) constructorArgumentTypes)) []]]


constraints :: [Name] -> [Pred]
constraints = map (ClassP ''Default . return . VarT)

instanceQ :: Bool -> Name -> [TyVarBndr] -> Name -> [Type] -> Q [Dec]
instanceQ b t vs c as = return <$> createInstance b t (map name vs) c as

name :: TyVarBndr -> Name
name (PlainTV n) = n
name (KindedTV n k) = n

deriveDefault :: Bool -> Name -> Q [Dec]
deriveDefault b n = do
	info <- reify n
	case info of
		TyConI (DataD _ qn tvars (con:_) _) -> case con of
			NormalC  conName ts -> instanceQ b qn tvars conName (map snd ts)
			RecC     conName ts -> instanceQ b qn tvars conName (map (\(v,s,t) -> t) ts)
			InfixC t conName t' -> instanceQ b qn tvars conName (map snd [t, t'])
			_ -> fail $ "Dunno how to derive Default instances for existential types"
		TyConI (DataD _ _ _ [] _) -> fail $ "Really? You want to derive a Default instance for an uninhabited type?"
		_ -> fail $ "Couldn't derive a Default instance; didn't know what to do with " ++ pprint info
