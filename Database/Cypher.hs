{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, ScopedTypeVariables, FlexibleInstances #-}
module Database.Cypher (
	Cypher,
	Entity(..),
	CypherResult(..),
	LuceneQuery,
	runCypher,
	cypher,
	cypherCreate,
	cypherGet,
	cypherSet,
	luceneEncode,
	withCypherManager,
	CypherException(..),
	DBInfo(..),
	Hostname,
	Port,
	CypherVal(..),
	CypherVals(..),
	CypherCol(..),
	CypherCols(..),
	CypherMaybe(..),
	CypherUnit(..)
	) where

import Database.Cypher.Lucene
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Network.HTTP.Conduit
import Network.HTTP.Types
import Data.Conduit
import Data.Typeable
import Data.Text (Text)
import Control.Exception
import Control.Applicative
import Control.Monad
import Data.Monoid
import Control.Monad.IO.Class
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

-- | Information about your neo4j configuration needed to make requests over the REST api.
data DBInfo = DBInfo {
	cypher_hostname :: Hostname,
	cypher_port :: Port
}

type Hostname = S.ByteString
type Port = Int
$(deriveJSON (drop 7) ''DBInfo)

-- | All interaction with Neo4j is done through the Cypher monad. Use 'cypher' to add a query to the monad.
newtype Cypher a = Cypher {
	uncypher :: ((DBInfo, Manager) -> ResourceT IO a)
}

-- | Raw result data returned by Neo4j. Only use this if you care about column headers.
data CypherResult a = CypherResult {
	rescolumns :: [Text],
	resdata :: a
} deriving (Show, Eq)

-- | A single result returned by Neo4j.
newtype CypherVal a = CypherVal a

-- | A single column returned by Neo4j.
newtype CypherCol a = CypherCol a

-- | Columns returned by Neo4j.
newtype CypherCols a = CypherCols a

-- | Values returned by Neo4j.
newtype CypherVals a = CypherVals [a]

-- | Possibly a value returned by Neo4j
data CypherMaybe a = CypherJust a | CypherNothing

-- | No value returned from Neo4j
data CypherUnit = CypherUnit

data CypherRequest = CypherRequest {
	req_query :: Text,
	req_params :: Value
} deriving (Show, Eq)

-- | A neo4j node or edge
data Entity a = Entity {
	entity_id :: Text,
	entity_data :: a
} deriving (Show, Eq)

instance FromJSON a => FromJSON (Entity a) where
	parseJSON (Object v) = Entity <$>
							v .: "self" <*>
							v .: "data"
	parseJSON _ = mempty

instance ToJSON (Entity a) where
	toJSON = toJSON . entity_id

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

instance MonadIO Cypher where
	liftIO f = Cypher $ const (liftIO f)

instance FromJSON a => FromJSON (CypherVal a) where
	parseJSON x = do
		(CypherResult _ [[d]]) <- parseJSON x
		return $ CypherVal d

instance FromJSON a => FromJSON (CypherCol a) where
	parseJSON x = do
		(CypherResult _ [d]) <- parseJSON x
		return $ CypherCol d

instance FromJSON a => FromJSON (CypherCols a) where
	parseJSON x = do
		(CypherResult _ d) <- parseJSON x
		return $ CypherCols d

instance FromJSON a => FromJSON (CypherVals a) where
	parseJSON x = do
		(CypherResult _ d) <- parseJSON x
		liftM CypherVals (mapM safeHead d)

instance FromJSON a => FromJSON (CypherMaybe a) where
	parseJSON x = do
		(CypherResult _ ds) <- parseJSON x
		case ds of
			[[d]] -> return $ CypherJust d
			_ -> return CypherNothing

instance FromJSON CypherUnit where parseJSON _ = return CypherUnit

safeHead :: [a] -> Parser a
safeHead [a] = return a
safeHead _ = mzero

-- | Perform a cypher query
cypher :: FromJSON a => Text -> Value -> Cypher a
cypher txt params = Cypher $ \(DBInfo h p, m)-> do
	let req = def { host = h, port = p,
					path = "db/data/cypher",
					requestBody = RequestBodyLBS (encode $ CypherRequest txt params),
					requestHeaders = headerAccept "application/json" : headerContentType "application/json" : requestHeaders def,
					method = "POST",
					checkStatus = (\_ _-> Nothing)
				  }
	r <- httpLbs req m
	let sci = statusCode (responseStatus r)
	if 200 <= sci && sci < 300
		then (case decode (responseBody r) of
				Nothing -> throwClientParse (responseBody r)
				Just x-> return x)
		else throw $ CypherServerException (responseStatus r) (responseHeaders r) (responseBody r)

-- | Get the http connection manager for a Cypher monad
withCypherManager :: (Manager -> ResourceT IO a) -> Cypher a
withCypherManager f = Cypher (\(_,m)-> f m)

-- | Create a node with the properties of the given datatype.
cypherCreate :: (ToJSON a, FromJSON b) => a -> Cypher b
cypherCreate obj = cypher "create (a {obj}) return a" $ object ["obj" .= obj]

-- | Set a node with properties matching the first argument to have the properties of the second.
cypherSet :: (ToJSON a, ToJSON a1, FromJSON b) => a -> a1 -> Cypher b
cypherSet a b = do
    (CypherVal x) <- cypher "start n=node({a}) set n = {b} return n" (object ["a" .= a, "b" .= b])
    return x

-- | Get the nodes matching the given lucene query
cypherGet :: (ToJSON a1, FromJSON a) => a1 -> Cypher a
cypherGet lc = cypher "start a = node:node_auto_index({lc}) return a" $ object ["lc" .= lc]

-- | Execute some number of cypher queries
runCypher :: Cypher a -> DBInfo -> Manager -> IO a
runCypher c dbi m =
	runResourceT $ do
    	uncypher c (dbi, m)

