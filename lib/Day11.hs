{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day11 where

import qualified Data.Attoparsec.Text as Atto

import qualified Data.Text.IO as Text
import Control.Monad ((<=<))
import Control.Lens

import qualified Data.Witherable as W
import Data.Monoid

import Data.Generics.Labels ()

import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import GHC.Generics (Generic)
import Data.Functor (($>))
import Control.Applicative (Alternative((<|>)))

data Chair = Occupied | Open deriving (Eq, Show, Generic)
data Space = Floor | Chair' Chair deriving (Eq, Show, Generic)

type Lobby = Seq.Seq (Seq.Seq Space)

parseSpace :: Atto.Parser Space
parseSpace = Atto.char '.' $> Floor
           <|> Atto.char 'L' $> Chair' Open
           <|> Atto.char '#' $> Chair' Occupied

parseRow :: Atto.Parser (Seq.Seq Space)
parseRow = Seq.fromList <$> Atto.many1' parseSpace

parseFile :: Atto.Parser Lobby
parseFile = Seq.fromList <$> parseRow `Atto.sepBy'` Atto.endOfLine

readInput :: FilePath -> IO Lobby
readInput = either fail pure
          . Atto.parseOnly parseFile
          <=< Text.readFile

neighbors :: Lobby -> (Int, Int) -> [Chair]
neighbors lobby (x,y) = W.mapMaybe (\(a,b) -> lobby ^? ix a . ix b . #_Chair') ns
  where
    ns = [(a,b) | a <- [x-1..x+1], b <- [y-1..y+1], (a,b) /= (x,y)]

step :: Int -> (Lobby -> (Int, Int) -> [Chair]) -> Lobby -> Lobby
step n f lobby = Seq.mapWithIndex (Seq.mapWithIndex . d) lobby
  where
    d _ _ Floor = Floor
    d x y (Chair' Occupied) = if n <= length (filter (== Occupied) $ f lobby (x,y))
      then Chair' Open else Chair' Occupied
    d x y (Chair' Open) = if Occupied `elem` f lobby (x,y)
      then Chair' Open else Chair' Occupied

fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint f x = maybe (error "list is infinite") fst . F.find (uncurry (==)) $ zip xs (tail xs)
  where
    xs = iterate f x

run :: (Lobby -> Lobby) -> Lobby -> Int
run f = getSum . foldMap (foldMap (Sum . fromEnum . (== Chair' Occupied))) . fixPoint f

partA :: Lobby -> Int
partA = run (step 4 neighbors)

cardinals :: [(Int, Int)]
cardinals = [(x,y) | x <- [-1,0,1], y <- [-1,0,1], (0,0) /= (x,y)]

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (a,b) (x,y) = (a+x, b+y)

seen :: Lobby -> (Int, Int) -> [Chair]
seen lobby (x,y) = W.mapMaybe (findD lobby (x,y)) cardinals

findD :: Lobby -> (Int, Int) -> (Int, Int) -> Maybe Chair
findD lobby (x,y) d = go $ move d (x,y)
  where
    go pt@(x,y) = f pt =<< lobby ^? ix x . ix y
    f pt Floor = go (move d pt)
    f _ (Chair' c) = pure c

showLobby :: Lobby -> String
showLobby = foldMap ((<> "\n") . foldMap show')
  where
    show' Floor = "."
    show' (Chair' Occupied) = "#"
    show' (Chair' Open) = "L"

partB :: Lobby -> Int
partB = run (step 5 seen)
