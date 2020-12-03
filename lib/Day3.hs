{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Day3 where

import GHC.Generics (Generic)

import Control.Lens (over, alaf, ala, (^?!), (%~), (&), ix, (^.), _1, _2, _Unwrapping)

import Data.Generics.Labels ()

import qualified Data.Attoparsec.Text as Atto

import Control.Applicative (liftA2,  (<|>))
import Control.Monad ((<=<))
import qualified Data.Text.IO as Text
import Data.Functor (($>))
import qualified Data.Foldable as F
import qualified Data.Witherable as W
import qualified Data.List as L
import Data.Monoid (Sum(Sum))
import Endo (Endo(Endo))
import Control.Arrow (Kleisli(Kleisli))

data Tile = Tree
          | Open
          deriving (Eq, Show, Generic)

data Forest = Forest
  { trees :: [[Tile]] -- would love dependent types here :(
  , location :: (Int, Int)
  , width :: Int
  , height :: Int
  } deriving (Eq, Show, Generic)

moveRight :: Forest -> Forest
moveRight forest = forest & #location . _1 %~ (`mod` forest ^. #width) . (+1)

moveDown :: Forest -> Maybe Forest
moveDown = W.filter (liftA2 (<) (^. #location . _2) (^. #height)) . Just . (#location . _2 %~ (+1))
--moveDown = W.filter (\f -> f ^. #location . _2 < f ^. #height) . Just . (#location . _2 %~ (+1))

getTile :: Forest -> Tile
getTile forest = forest ^?! #trees . ix (forest ^. #location . _2) . ix (forest ^. #location . _1)

atBottom :: Forest -> Bool
atBottom forest = forest ^. #height == forest ^. #location . _2

parseTile :: Atto.Parser Tile
parseTile = Atto.char '.' $> Open
          <|> Atto.char '#' $> Tree

parseRow :: Atto.Parser [Tile]
parseRow = Atto.many1' parseTile

parseGrid :: Atto.Parser [[Tile]]
parseGrid = parseRow `Atto.sepBy` Atto.endOfLine

initForest :: [[Tile]] -> Forest
initForest tiles = Forest tiles (0,0) (F.length $ head tiles) (F.length tiles)

readInput :: FilePath -> IO Forest
readInput = either fail (pure . initForest)
          . Atto.parseOnly parseGrid
          <=< Text.readFile

treesOnPath :: Int -> Int -> Forest -> Int
treesOnPath r d = alaf Sum foldMap (fromEnum . hasTree) . L.unfoldr (fmap dup . move)
  where
    dup x = (x,x)
    applyN n = ala Endo foldMap . L.replicate n
    move = over (_Unwrapping Kleisli) (applyN d) moveDown . applyN r moveRight
    hasTree = (== Tree) . getTile

partA :: Forest -> Int
partA = treesOnPath 3 1

partB :: Forest -> Int
partB = product . traverse (uncurry treesOnPath) [(1,1), (3,1), (5,1), (7,1), (1,2)]
