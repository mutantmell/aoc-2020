{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Day1 where

import Data.Text (Text)
import qualified Data.Text.Read as TR

import Control.Monad ( (<=<), foldM )
import Control.Monad.State ( evalState, State )

import GHC.Generics ( Generic )

import Data.Generics.Labels ()
import Control.Lens ((%=), use)
import qualified Data.Foldable as F
import qualified Common
import Data.Maybe (listToMaybe)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

readInput :: FilePath -> IO [Int]
readInput = either fail pure
          . traverse (fmap fst . TR.decimal)
          <=< Common.parseLines

newtype S1 = S1
  { seen :: [Int]
  } deriving (Eq, Show, Generic)

emptyState1 :: S1
emptyState1 = S1 []

partA :: [Int] -> Either Text Int
partA = maybe (Left "unknown value") (Right . uncurry (*))
      . listToMaybe
      . flip evalState emptyState1
      . foldM f []
  where
    f :: [(Int, Int)] -> Int -> State S1 [(Int, Int)]
    f acc int = do
        seen <- use #seen
        #seen %= (int:)
        let wanted = 2020 - int
        if wanted `F.elem` seen
            then pure $ (wanted, int):acc
            else pure acc

partA' :: [Int] -> Either Text Int
partA' = maybe (Left "unknown value") Right .listToMaybe . go [] []
  where
    go :: [Int] -> [Int] -> [Int] -> [Int]
    go _ ans [] = ans
    go seen ans (x:xs) = go (x:seen) ans' xs
      where
        w = 2020 - x
        ans' = if w `F.elem` seen then w*x:ans else ans

data S2 = S2
  { seen :: [Int]
  , seenPairs :: IntMap (Int, Int)
  } deriving (Eq, Show, Generic)

emptyState2 :: S2
emptyState2 = S2 [] IntMap.empty

partB :: [Int] -> Either Text Int
partB = maybe (Left "unknown value") Right
      . listToMaybe
      . flip evalState emptyState2
      . foldM f []
  where
    f :: [Int] -> Int -> State S2 [Int]
    f acc int = do
        seen' <- use #seen
        #seen %= (int:)
        seenPairs' <- use #seenPairs
        #seenPairs %= flip (F.foldl' (\map x -> IntMap.insert (2020 - x - int) (x,int) map)) seen'
        case IntMap.lookup int seenPairs' of
            Just (x,y) -> pure $ (x * y * int):acc
            Nothing -> pure acc