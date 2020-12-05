{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Day5 where

import qualified Data.Text.IO as Text

import GHC.Generics (Generic)

import Data.Generics.Labels ()

import qualified Data.Attoparsec.Text as Atto

import Control.Monad ((<=<))
import Data.Functor (($>))
import Control.Applicative (Alternative((<|>)))
import qualified Data.Foldable as F

import qualified Data.Witherable as W

data Split = Lower | Higher deriving (Eq, Show, Generic)
type SplitRow = Split
type SplitCol = Split

data BoardingPass = BoardingPass
  { row :: [SplitRow]
  , col :: [SplitCol]
  } deriving (Eq, Show, Generic)
  
parseSplitRow :: Atto.Parser Split
parseSplitRow = Atto.char 'F' $> Lower
              <|> Atto.char 'B' $> Higher

parseSplitCol :: Atto.Parser Split
parseSplitCol = Atto.char 'L' $> Lower
              <|> Atto.char 'R' $> Higher

parseBoardingBass :: Atto.Parser BoardingPass
parseBoardingBass = BoardingPass <$> Atto.many1' parseSplitRow <*> Atto.many1' parseSplitCol

parseFile :: Atto.Parser [BoardingPass]
parseFile = parseBoardingBass `Atto.sepBy'` Atto.endOfLine

readInput :: FilePath -> IO [BoardingPass]
readInput = either fail pure
          . Atto.parseOnly parseFile
          <=< Text.readFile

numRows :: Int
numRows = 128
numCols :: Int
numCols = 8

upTo :: Int -> Int -> Interval
upTo x y = (x, y-1)

type Interval = (Int, Int)

midP :: Interval -> Int
midP (x,y) = (x + y) `div` 2

split :: (Int, Int) -> Split -> (Int, Int)
split i@(x,_) Lower = (x, midP i)
split i@(_,y) Higher = (midP i + 1, y)

converge :: (Int, Int) -> Maybe Int
converge (x,y) | x == y = Just x
               | otherwise = Nothing

splits :: (Int, Int) -> [Split] -> Maybe Int
splits init = converge . F.foldl' split init

getRow :: BoardingPass -> Maybe Int
getRow = splits (0 `upTo` numRows) . row

getCol :: BoardingPass -> Maybe Int
getCol = splits (0 `upTo` numCols) . col

getSeatNo :: BoardingPass -> Maybe Int
getSeatNo pass = do
  r <- getRow pass
  c <- getCol pass
  pure $ (8*r) + c

partA :: [BoardingPass] -> Int
partA = F.maximum . W.mapMaybe getSeatNo

partB :: [BoardingPass] -> Int
partB bps = head $ filter (`notElem` seatNos) [ midP (x,y) | x <- seatNos, y <- seatNos, x - y == 2 ]
  where
    seatNos = W.mapMaybe getSeatNo bps
