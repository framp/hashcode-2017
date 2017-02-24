module InputParser where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8
import Type

parseState :: Parser State
parseState = do
        metadata <- parseMetadata
        videos <- parseVideos
        endpoints <- many' parseEndpoint
        requests <- many' parseRequest
        let caches = map (\n -> cacheSize metadata) [0..(cachesNumber metadata)]
        return $ State metadata videos endpoints requests caches []

parseMetadata :: Parser Metadata
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
        return $ Metadata videosNumber endpointsNumber requestsNumber cachesNumber cacheSize

parseVideos :: Parser [Video]
parseVideos = do
        videos <- decimal `sepBy` (char ' ')
        char '\n'
        return videos

parseEndpoint :: Parser Endpoint
parseEndpoint = do
        datacenterLatency <- decimal
        char ' '
        cacheNumber <- decimal
        char '\n'
        caches <- count cacheNumber parseEndpointCache
        return $ Endpoint datacenterLatency caches

parseEndpointCache :: Parser (Int, Int)
parseEndpointCache = do
        id <- decimal
        char ' '
        latency <- decimal
        char '\n'
        return $ (id, latency)

parseRequest :: Parser Request
parseRequest = do
        video <- decimal
        char ' '
        endpoint <- decimal
        char ' '
        size <- decimal
        char '\n'
        return $ Request size video endpoint

parse :: BS.ByteString -> State
parse content = either (const emptyState) id $ parseOnly parseState content

emptyState :: State
emptyState = State (Metadata 0 0 0 0 0) [] [] [] [] []