{-# LANGUAGE DuplicateRecordFields #-}
module Type where

import qualified Data.IntMap as IntMap

data State = State 
        { metadata :: Metadata 
        , videos :: [Video]
        , endpoints :: [Endpoint]
        , requests :: [Request]
        , caches :: [(CacheIndex, Cache)]
        , allocations :: IntMap.IntMap [VideoIndex] } deriving (Show)

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
        , videoIndex :: VideoIndex
        , endpointIndex :: EndpointIndex } deriving (Show)

data Problem = Problem
        { cacheIndex :: CacheIndex
        , score :: Score
        , requestIndex :: RequestIndex
        , videoIndex :: VideoIndex 
        , video :: Video }

type Video = Int
type Latency = Int
type Cache = Int
type Score = Float
type RequestIndex = Int
type VideoIndex = Int
type CacheIndex = Int
type EndpointIndex = Int
