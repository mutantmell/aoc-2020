{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day9 where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Foldable as F

import Data.Generics.Labels ()

import qualified Data.Text.IO as Text
import Control.Monad ((<=<))
import Control.Lens
import Control.Applicative
import Data.Maybe (maybeToList)
import qualified Data.List as L

type Preamble = [Int]

parseFile :: (Integral a) => Atto.Parser [a]
parseFile = Atto.decimal `Atto.sepBy'` Atto.endOfLine

readInput :: (Integral a) => FilePath -> IO [a]
readInput = either fail pure
          . Atto.parseOnly parseFile
          <=< Text.readFile

runs :: Int -> [a] -> [([a], a)]
runs n = maybeToList . unsnoc . take (n+1) <=< L.tails

pairs :: [Int] -> [Int]
pairs n = [ x+y | x<-n, y<-n ]

notSumPair :: [Int] -> Int -> Bool
notSumPair pairs v = not $ F.any (== v) pairs

runA :: Int -> [Int] -> Int
runA preSize = maybe (-1) snd . F.find (uncurry notSumPair) . over (traverse._1) pairs . runs preSize

partA :: [Int] -> Int
partA = runA 25

partA'soln :: Integer
partA'soln = 26796446

runB :: Integer -> [Integer] -> (Integer, Integer)
runB v = maybe (-1,-1) (liftA2 (,) L.maximum L.minimum) . F.find ((== v) . sum) . filter (not . null) . (L.tails <=< L.inits)

partB :: [Integer] -> Integer
partB = uncurry (+) . runB partA'soln
