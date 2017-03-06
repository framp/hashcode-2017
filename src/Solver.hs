{-# LANGUAGE DuplicateRecordFields #-}
module Solver where

import Data.List (sortOn)
import Debug.Trace
import qualified Data.IntMap as IntMap
import qualified Type as T

makeProblems :: T.State -> (Int, T.Request) -> [T.Problem]
makeProblems state (requestIndex, request) = 
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

makeAllocations :: T.State -> IntMap.IntMap [T.VideoIndex]
makeAllocations state = 
  let 
    problems = foldl (++) [] $ map (makeProblems state) $ zip [0..] (T.requests state)
    sortByScore = sortOn $ (* (-1)) . T.score
    --TODO: Check videoSize against cacheSize
    solveProblem problem = IntMap.insertWith (++) (T.cacheIndex problem) [T.videoIndex (problem :: T.Problem)] 
  in 
    foldl (flip solveProblem) IntMap.empty (sortByScore problems)

solve :: T.State -> T.State
solve state = state { T.allocations = makeAllocations state }