{-# LANGUAGE OverloadedStrings #-}

import System.IO  
import System.Environment
import qualified Data.ByteString as BS

import InputParser (parse)
import Solver (solve)
import OutputEncoder (encode)

main :: IO ()
main = do  
        args <- getArgs
        content <- BS.readFile $ args !! 0
        putStr . encode . solve . parse $ content
