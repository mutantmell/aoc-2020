{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day10 where

import qualified Data.Attoparsec.Text as Atto

import qualified Data.Text.IO as Text
import Control.Monad ((<=<))
import Control.Lens

import qualified Data.Tree as Tree
import Data.List (tails, sort)
import qualified Data.Witherable as W
import Data.Monoid

import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import qualified Data.IntMap as IntMap
import Data.Maybe

type Chains = Tree.Tree Int
type Chain = [Int]

parseFile :: (Integral a) => Atto.Parser [a]
parseFile = Atto.decimal `Atto.sepBy'` Atto.endOfLine

readInput :: (Integral a) => FilePath -> IO [a]
readInput = either fail pure
          . Atto.parseOnly parseFile
          <=< Text.readFile

mkChain :: [Int] -> [Int]
mkChain = f . sort
  where
    f xs = 0:xs ++ [last xs + 3]

jumps :: [Int] -> (Int, Int, Int)
jumps = go (0,0,0)
  where
    go counts (x:zs@(y:_)) = go (incr counts (y-x)) zs
    go counts _ = counts
    incr (x,y,z) 1 = (x+1,y,z)
    incr (x,y,z) 2 = (x,y+1,z)
    incr (x,y,z) 3 = (x,y,z+1)
    incr _ _ = error "wat"

partA :: [Int] -> Int
partA = (\(x,_,z) -> x*z) . jumps . mkChain

asChains :: [Int] -> Chains
asChains list = Tree.unfoldTree f (0, sorted)
  where
    sorted = sort list ++ [maximum list + 3]

    f :: (Int, [Int]) -> (Int, [(Int, [Int])])
    f (x, []) = (x, [])
    f (x, [y]) = (x, [(y,[])])
    f (x, ys) = (x, filter ((<= 3) . subtract x . fst) . W.mapMaybe uncons $ tails ys)


asChains' :: [Int] -> Tree.Tree Integer
asChains' list = Tree.unfoldTree f (0, sorted)
  where
    sorted = sort list ++ [maximum list + 3]

    f :: (Int, [Int]) -> (Integer, [(Int, [Int])])
    f (_, []) = (1, [])
    f (_, [y]) = (0, [(y,[])])
    f (x, ys) = (0, filter ((<= 3) . subtract x . fst) . W.mapMaybe uncons $ tails ys)

validChains'' :: Seq.Seq Int -> Integer
validChains'' list = go' (0, sorted) `div` 2
  where
    sorted = Seq.sort list Seq.|> F.maximum list + 3
    go' (_, Seq.Empty) = 1
    go' (_, _ Seq.:<| Seq.Empty) = 1
    go' (x, y Seq.:<| ys)
      | y - x <= 3 = go' (y, ys) + go' (x, ys)
      | otherwise = 0

validChains4 :: Seq.Seq Int -> Integer
validChains4 list = sumChains (F.maximum list + 3) list
  where
    sumChains n = fromMaybe (-1) . IntMap.lookup 0 . foldr f (IntMap.singleton n 1) . (0 Seq.<|) . Seq.sort

    f :: Int -> IntMap.IntMap Integer -> IntMap.IntMap Integer
    f n acc = IntMap.insert n (sum $ fromMaybe 0 . flip IntMap.lookup acc <$> [n+1, n+2, n+3]) acc

{-
allLists :: Tree.Tree a -> [[a]]
allLists = Tree.foldTree f
  where
    f :: a -> [[[a]]] -> [[a]]
    f root [] = [[root]]
    f root xs = (root:) <$> concat xs
-}

allLists :: Int -> Chains -> [Chain]
allLists n = Tree.foldTree f
  where
    f :: Int -> [[Chain]] -> [Chain]
    f root []
      | root == n = [[root]]
      | otherwise = []
    f root xs = (root:) <$> concat xs


validChains :: Int -> Chains -> Integer
validChains n = Tree.foldTree f
  where
    f :: Int -> [Integer] -> Integer
    f root []
      | root == n = 1
      | otherwise = 0
    f _ xs = sum xs

validChains' :: Tree.Tree Integer -> Integer
validChains' = ala Sum foldMap

endsWith :: Int -> [Int] -> Bool
endsWith _ [] = False
endsWith n xs = last xs == n

partB :: [Int] -> Integer
partB = validChains4 . Seq.fromList
