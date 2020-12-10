module Main where

import qualified Day10
import System.Environment

main :: IO ()
main = getArgs >>= \(f:_) -> (Day10.partB <$> Day10.readInput f) >>= print
