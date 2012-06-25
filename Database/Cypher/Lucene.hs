{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- Code shamelessly copied from Data.Aeson.Encode by MailRank, Inc.

module Database.Cypher.Lucene (luceneEncode) where
import Data.Aeson.Types (ToJSON(..), Value(..))
import Data.Attoparsec.Number (Number(..))
import Data.Monoid
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Numeric (showHex)
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V


fromValue :: Value -> Builder
fromValue Null = mempty
fromValue (Bool b) = if b then "true" else "false"
fromValue (Number n) = fromNumber n
fromValue (String s) = string s
fromValue (Array v)
    | V.null v = mempty
    | otherwise = singleton '(' <>
                  fromValue (V.unsafeHead v) <>
                  V.foldr f (singleton ')') (V.unsafeTail v)
  where f a z = " OR " <> fromValue a <> z
fromValue (Object m) =
    case H.toList m of
      (x:xs) -> one x <> foldr f mempty xs
      _      -> mempty
  where f a z     = let n = one a in if n == "" then z else " AND " <> n <> z
        one (k,v) = let n = fromValue v in if n == "" then n else fromText k <> singleton ':' <> n

string :: T.Text -> Builder
string s = singleton '"' <> quote s <> singleton '"'
  where
    quote q = case T.uncons t of
                Nothing     -> fromText h
                Just (!c,t') -> fromText h <> escape c <> quote t'
        where (h,t) = {-# SCC "break" #-} T.break isEscape q
    isEscape c = c == '\"' || c == '\\' || c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c
        | c < '\x20' = fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = singleton c
        where h = showHex (fromEnum c) ""

fromNumber :: Number -> Builder
fromNumber (I i) = decimal i
fromNumber (D d)
    | isNaN d || isInfinite d = "null"
    | otherwise               = realFloat d

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
luceneEncode :: ToJSON a => a -> L.ByteString
luceneEncode = encodeUtf8 . toLazyText . fromValue . toJSON
