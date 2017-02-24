{-# LANGUAGE DuplicateRecordFields #-}

module Type where

data State = State 
        { metadata :: Metadata 
        , videos :: [Video]
        , endpoints :: [Endpoint]
        , requests :: [Request]
        , caches :: [Cache]
        , allocations :: [Allocation] } deriving (Show)

data Metadata = Metadata 
        { videosNumber :: Int
        , endpointsNumber :: Int
        , requestsNumber :: Int
        , cachesNumber :: Int
        , cacheSize :: Int } deriving (Show)

data Endpoint = Endpoint 
        { dataCenterLatency :: Int  
        , cachesLatencies :: [(CacheIndex, Latency)] } deriving (Show)

data Request = Request
        { size :: Int
        , video :: Video
        , endpoint :: EndpointIndex } deriving (Show)

data Allocation = Allocation 
        { video :: Video 
        , cache :: CacheIndex } deriving (Show)

type Video = Int
type Latency = Int
type Cache = Int
type CacheIndex = Int
type EndpointIndex = Int
