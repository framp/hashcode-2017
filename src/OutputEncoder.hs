module OutputEncoder where

import Data.List (intercalate)
import qualified Type as T
import qualified Data.IntMap as IntMap

encode :: T.State -> String
encode state = 
  let 
    allocations = T.allocations state
    row acc key value = acc ++ (show key) ++ " " ++ (intercalate " " $ map show value) ++ "\n"
    size = show $ IntMap.size allocations
    rows = IntMap.foldlWithKey row "" allocations
  in 
    size ++ "\n" ++ rows