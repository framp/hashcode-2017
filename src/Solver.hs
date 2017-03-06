{-# LANGUAGE DuplicateRecordFields #-}
module Solver where

import Data.List (sortOn)
import Debug.Trace
import qualified Data.IntMap as IntMap
import qualified Type as T

requestProblems :: T.State -> (Int, T.Request) -> [T.Problem]
requestProblems state (requestIndex, request) = 
  let
    requestSize = T.size request
    videoIndex = T.videoIndex (request :: T.Request)
    videoSize = (T.videos state) !! videoIndex
    endpoint = (T.endpoints state) !! (T.endpointIndex request)
    latencySaving latency = (T.dataCenterLatency endpoint) - latency
    score latency = (fromIntegral (requestSize * (latencySaving latency))) / (fromIntegral videoSize)
    makeProblem (cacheIndex, latency) = T.Problem cacheIndex (score latency) requestIndex videoIndex videoSize
  in
    map makeProblem (T.cachesLatencies endpoint)

insertRequests :: [T.Problem] -> (T.CacheIndex, T.Cache) -> IntMap.IntMap [T.VideoIndex]
insertRequests problems (cacheIndex, cacheSize) =
  let
    filterByCache = filter $ (== cacheIndex) . T.cacheIndex
    sortByScore = sortOn $ (* (-1)) . T.score
    -- Add check if req is already satisfied, bigger score wins
    -- Check size
    insertRequest problem = IntMap.insertWith (++) (T.cacheIndex problem) [T.videoIndex (problem :: T.Problem)] 
  in
    foldl (flip insertRequest) IntMap.empty ((sortByScore . filterByCache) problems)

makeAllocations :: T.State -> IntMap.IntMap [T.VideoIndex]
makeAllocations state = 
  let 
    problems = foldl (++) [] $ map (requestProblems state) $ zip [0..] (T.requests state)
    cacheAllocationMaps = map (insertRequests problems) $ T.caches state
  in 
    foldl (IntMap.unionWith (++)) IntMap.empty cacheAllocationMaps

solve :: T.State -> T.State
solve state = state { T.allocations = makeAllocations state }