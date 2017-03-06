module InputParser where

import qualified Data.ByteString as BS
import qualified Data.IntMap as IntMap
import Data.Attoparsec.ByteString.Char8
import qualified Type as T

parseState :: Parser T.State
parseState = do
        metadata <- parseMetadata
        videos <- parseVideos
        endpoints <- many' parseEndpoint
        requests <- many' parseRequest
        let caches = map (\n -> (n, T.cacheSize metadata)) [0..(T.cachesNumber metadata)]
        return $ T.State metadata videos endpoints requests caches IntMap.empty

parseMetadata :: Parser T.Metadata
parseMetadata = do
        videosNumber <- decimal
        char ' '
        endpointsNumber <- decimal
        char ' '
        requestsNumber <- decimal
        char ' '
        cachesNumber <- decimal
        char ' '
        cacheSize <- decimal
        char '\n'
        return $ T.Metadata videosNumber endpointsNumber requestsNumber cachesNumber cacheSize

parseVideos :: Parser [T.Video]
parseVideos = do
        videos <- decimal `sepBy` (char ' ')
        char '\n'
        return videos

parseEndpoint :: Parser T.Endpoint
parseEndpoint = do
        datacenterLatency <- decimal
        char ' '
        cacheNumber <- decimal
        char '\n'
        caches <- count cacheNumber parseEndpointCache
        return $ T.Endpoint datacenterLatency caches

parseEndpointCache :: Parser (Int, Int)
parseEndpointCache = do
        id <- decimal
        char ' '
        latency <- decimal
        char '\n'
        return $ (id, latency)

parseRequest :: Parser T.Request
parseRequest = do
        video <- decimal
        char ' '
        endpoint <- decimal
        char ' '
        size <- decimal
        char '\n'
        return $ T.Request size video endpoint

parse :: BS.ByteString -> T.State
parse content = either (const emptyState) id $ parseOnly parseState content

emptyState :: T.State
emptyState = T.State (T.Metadata 0 0 0 0 0) [] [] [] [] IntMap.empty