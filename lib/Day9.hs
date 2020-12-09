{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day9 where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Foldable as F

import qualified Data.Text.IO as Text
import Control.Monad ((<=<))
import Control.Lens
import Control.Applicative
import Data.Maybe (maybeToList)
import qualified Data.List as L

type Preamble a = [a]

parseFile :: (Integral a) => Atto.Parser [a]
parseFile = Atto.decimal `Atto.sepBy'` Atto.endOfLine

readInput :: (Integral a) => FilePath -> IO [a]
readInput = either fail pure
          . Atto.parseOnly parseFile
          <=< Text.readFile

runs :: (Integral a) => Int -> [a] -> [(Preamble a, a)]
runs n = maybeToList . unsnoc . take (n+1) <=< L.tails

pairs :: (Integral a) => [a] -> [a]
pairs n = [ x+y | x<-n, y<-n ]

notSumPair :: (Integral a) => [a] -> a -> Bool
notSumPair pairs v = F.all (/= v) pairs

runA :: (Integral a) => Int -> [a] -> a
runA preSize = maybe (-1) snd
             . F.find (uncurry notSumPair)
             . over (traverse . _1) pairs
             . runs preSize

partA :: [Integer] -> Integer
partA = runA 25

partA'soln :: Integer
partA'soln = 26796446

runB :: (Integral a) => a -> [a] -> (a, a)
runB v = maybe (-1,-1) (liftA2 (,) L.maximum L.minimum)
       . F.find ((== v) . sum)
       . (L.tails <=< L.inits)

partB :: [Integer] -> Integer
-- partB = uncurry (+) . (runB =<< runA 25)
partB = uncurry (+) . runB partA'soln
