{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, ScopedTypeVariables, FlexibleInstances #-}
module Database.Cypher (
	Cypher,
	Entity,
	CypherResult(..),
	runCypher,
	cypher,
	CypherException(..),
	Hostname,
	Port,
	OneTuple(..)
	) where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Network.HTTP.Conduit
import Network.HTTP.Types
import Data.Conduit
import Data.Typeable
import Data.Text (Text)
import Data.Tuple.OneTuple
import Control.Exception
import Control.Applicative
import Control.Monad
import Data.Monoid
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

data DBInfo = DBInfo Hostname Port Manager
type Hostname = S.ByteString
type Port = Int

-- | All interaction with Neo4j is done through the Cypher monad. Use "cypher" to add a query to the monad.
newtype Cypher a = Cypher {
	uncypher :: (DBInfo -> ResourceT IO a)
}

-- | Raw result data returned by Neo4j. Only use this if you care about column headers.
data CypherResult a = CypherResult {
	rescolumns :: [Text],
	resdata :: a
} deriving (Show, Eq)

data CypherRequest = CypherRequest {
	req_query :: Text,
	req_params :: Value
} deriving (Show, Eq)

-- | A neo4j node or edge
data Entity a = Entity {
	entity_id :: Text,
	entity_data :: a
} deriving (Show, Eq)

-- Ok, so they aren't inverses. But this is more useful.

instance FromJSON a => FromJSON (Entity a) where
	parseJSON (Object v) = Entity <$>
							v .: "self" <*>
							v .: "data"
	parseJSON _ = mempty

instance ToJSON (Entity a) where
	toJSON = toJSON . entity_id

instance FromJSON a => FromJSON (OneTuple a) where
	parseJSON x = do
		[l] <- parseJSON x
		return $ OneTuple l

instance ToJSON a => ToJSON (OneTuple a) where
	toJSON = toJSON . (\x->[x])

$(deriveJSON (drop 3) ''CypherResult)
$(deriveJSON (drop 4) ''CypherRequest)

-- | An error in handling a Cypher query, either in communicating with the server or parsing the result
data CypherException = CypherServerException Status ResponseHeaders L.ByteString | 
					   CypherClientParseException S.ByteString deriving (Show, Typeable)
instance Exception CypherException

throwClientParse bs = throw $ CypherClientParseException $ S.concat $ L.toChunks bs

instance Monad Cypher where
	return a = Cypher (const (return a))
	(Cypher  cmd) >>= f =
		Cypher $ \con-> do
			a <- cmd con
			uncypher (f a) con

class FromCypher a where
	fromCypher :: L.ByteString -> a

instance FromCypher () where
	fromCypher _ = ()

instance FromJSON a => FromCypher (CypherResult a) where
	fromCypher bs = 
		case decode bs of
			Just x -> x
			Nothing -> throwClientParse bs

instance FromJSON a => FromCypher [a] where
	fromCypher bs =
 		case decode bs of
			Just (CypherResult _ ds) -> ds
			_ -> throwClientParse bs

instance FromJSON a => FromCypher (OneTuple a) where
	fromCypher bs =
		case decode bs of
			Just (CypherResult _ [d]) -> d
			_ -> throwClientParse bs

instance (FromJSON a, FromJSON b) => FromCypher (a,b) where
	fromCypher bs =
		case decode bs of
			Just (CypherResult _ [d]) -> d
			_ -> throwClientParse bs

instance (FromJSON a, FromJSON b, FromJSON c) => FromCypher (a,b,c) where
	fromCypher bs =
		case decode bs of
			Just (CypherResult _ [d]) -> d
			_ -> throwClientParse bs

instance FromJSON a => FromCypher (Maybe a) where
	fromCypher bs =
		case decode bs of
			Just (CypherResult _ [a]) -> Just a
			Just (CypherResult _ []) -> Nothing
			_ -> throwClientParse bs

-- | Perform a cypher query
cypher :: FromCypher a => Text -> Value -> Cypher a
cypher txt params = Cypher $ \(DBInfo h p m)-> do
	let req = def { host = h, port = p,
					path = "db/data/cypher",
					requestBody = RequestBodyLBS (encode $ CypherRequest txt params),
					requestHeaders = headerAccept "application/json" : headerContentType "application/json" : requestHeaders def,
					method = "POST",
					checkStatus = (\_ _-> Nothing)
				  }
	r <- httpLbs req m
	let sci = statusCode (responseStatus r)
	if 200 <= sci && sci < 300 then return (fromCypher (responseBody r))
		else throw $ CypherServerException (responseStatus r) (responseHeaders r) (responseBody r)

-- | Execute some number of cypher queries
runCypher :: Cypher a -> Hostname -> Port -> IO a
runCypher c h p =
	runResourceT $ do
    	manager <- liftIO $ newManager def
    	uncypher c (DBInfo h p manager)

