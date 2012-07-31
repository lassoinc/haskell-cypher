{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}
module Database.Cypher (
	Cypher,
	Entity(..),
	CypherResult(..),
	LuceneQuery,
	runCypher,
	forkCypher,
	cypher,
	cypherGetNode,
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
import Control.Exception hiding (try, throwIO)
import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import Data.Monoid
import Control.Monad.IO.Class
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.HashMap.Strict as H
import Data.Text.Lazy.Builder
import Data.Aeson.Encode
import Data.List (elemIndices)
import Control.Monad.Trans.Resource
import Control.Monad.Base
import Control.Monad.Parallel (Parallel(..), Fork(..), parallelIO)

-- | Information about your neo4j configuration needed to make requests over the REST api.
data DBInfo = DBInfo {
	cypher_hostname :: Hostname,
	cypher_port :: Port
} deriving (Show, Eq)

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
newtype CypherVal a = CypherVal a deriving (Eq, Show)

-- | A single column returned by Neo4j.
newtype CypherCol a = CypherCol a deriving (Eq, Show)

-- | Columns returned by Neo4j.
newtype CypherCols a = CypherCols a deriving (Eq, Show)

-- | Values returned by Neo4j.
newtype CypherVals a = CypherVals [a] deriving (Eq, Show)

-- | Possibly a value returned by Neo4j
data CypherMaybe a = CypherJust a | CypherNothing deriving (Eq, Show)

-- | No value returned from Neo4j
data CypherUnit = CypherUnit deriving (Show)

data CypherRequest = CypherRequest {
	req_query :: Text,
	req_params :: Value
} deriving (Show, Eq)

-- | A neo4j node or edge
data Entity a = Entity {
	entity_id :: String,
	entity_properties :: String,
	entity_data :: a
} deriving (Show, Eq)

instance FromJSON a => FromJSON (Entity a) where
	parseJSON (Object v) = Entity <$>
							v .: "self" <*>
							v .: "properties" <*>
							v .: "data"
	parseJSON _ = mempty

instance ToJSON (Entity a) where
	toJSON a = toJSON (read x :: Int) where
		(_, _:x) = splitAt (last (elemIndices '/' (entity_id a))) (entity_id a)

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

-- | Create a cypher node
cypherCreate :: (ToJSON a, FromJSON b) => a -> Cypher b
cypherCreate obj = Cypher $ \(DBInfo h p, m)-> do
	let req = def { host = h, port = p,
					path = "db/data/node",
					requestBody = RequestBodyLBS (encode obj),
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

-- | Get a cypher node
cypherGetNode :: FromJSON b => Entity b -> Cypher (Entity b)
cypherGetNode e = Cypher $ \(DBInfo h p, m)-> do
	req <- liftIO $ parseUrl (entity_id e)
	let req' = req { host = h, port = p,
					requestHeaders = headerAccept "application/json" : headerContentType "application/json" : requestHeaders def,
					method = "GET",
					checkStatus = (\_ _-> Nothing)
				  }
	r <- httpLbs req m
	let sci = statusCode (responseStatus r)
	if 200 <= sci && sci < 300
		then (case decode (responseBody r) of
				Nothing -> throwClientParse (responseBody r)
				Just x-> return x)
		else throw $ CypherServerException (responseStatus r) (responseHeaders r) (responseBody r)

-- | Set cypher properties. This currently cannot be done through cypher queries.
cypherSet :: (ToJSON a, ToJSON a1) => (Entity a) -> a1 -> Cypher ()
cypherSet e obj = Cypher $ \(DBInfo h p, m)-> do
	let Object o1 = toJSON (entity_data e)
	let Object o2 = toJSON obj
	let body = RequestBodyLBS $ encodeUtf8 $ toLazyText $ fromValue $ Object (o2 `H.union` o1)
	req <- liftIO $ parseUrl (entity_properties e)
	let req' = req { host = h, port = p,
					requestBody = body,
					requestHeaders = headerAccept "application/json" : headerContentType "application/json" : requestHeaders def,
					method = "PUT",
					checkStatus = (\_ _-> Nothing)
				  }
	r <- httpLbs req' m
	let sci = statusCode (responseStatus r)
	if 200 <= sci && sci < 300
		then return ()
		else (let e = CypherServerException (responseStatus r) (responseHeaders r) (responseBody r)
			 in throw e)

-- | Get the nodes matching the given lucene query
cypherGet :: (ToJSON a1, FromJSON a) => a1 -> Cypher a
cypherGet lc = cypher "start a = node:node_auto_index({lc}) return a" $ object ["lc" .= lc]

-- | Get the http connection manager for a Cypher monad
withCypherManager :: (Manager -> ResourceT IO a) -> Cypher a
withCypherManager f = Cypher (\(_,m)-> f m)

-- | Execute some number of cypher queries
runCypher :: Cypher a -> DBInfo -> Manager -> IO a
runCypher c dbi m =
	runResourceT $ do
    	uncypher c (dbi, m)

-- | Execute a request in a separate thread
forkCypher :: Cypher () -> Cypher ()
forkCypher (Cypher cmd) = Cypher (\d-> resourceForkIO (cmd d) >> return ())

instance Fork Cypher where
	forkExec (Cypher c) = Cypher $ \d-> do
		c' <- forkExec (c d)
		return $ Cypher (const c')

instance Parallel Cypher where
   bindM2 = parallelIO

instance MonadBase (ResourceT IO) Cypher where
	liftBase = Cypher . const

instance Functor Cypher where
	fmap g (Cypher f) = Cypher $ \d-> do
		arg <- f d
		return $ g arg

instance Applicative Cypher where
	pure = Cypher . const . return
	Cypher f <*> Cypher x = Cypher $ \d-> do
		func <- f d
		arg <- x d
		return $ func arg
