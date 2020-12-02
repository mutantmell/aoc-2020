{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Day2 where

import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics (Generic)

import Control.Lens ((^?), ix, (^.))

import Data.Generics.Labels ()
import Data.Generics.Product ()
import qualified Common

import qualified Data.Foldable as F

import qualified Data.Attoparsec.Text as Atto

import qualified Data.Char as Char
import Data.Monoid
import Control.Applicative

data Policy = Policy
  { lower :: Int
  , upper :: Int
  , char :: Char
  } deriving (Eq, Show, Generic)

type Password = Text

parsePolicy :: Atto.Parser Policy
parsePolicy = do
  lower <- Atto.decimal
  _ <- Atto.char '-'
  upper <- Atto.decimal
  _ <- Atto.char ' '
  char <- Atto.satisfy Char.isAlpha
  pure $ Policy{..}

parsePassword :: Atto.Parser Password
parsePassword = Atto.takeWhile (not . Atto.isEndOfLine)

parseLine :: Atto.Parser (Policy, Password)
parseLine = do
  policy <- parsePolicy
  _ <- Atto.string ": "
  password <- parsePassword
  pure (policy, password)

readInput :: FilePath -> IO [(Policy, Password)]
readInput path = either fail pure . traverse (Atto.parseOnly parseLine) =<< Common.parseFile path

partA :: [(Policy, Password)] -> Int
partA = getSum . foldMap (Sum . fromEnum . satisfies)
  where
    satisfies (policy, password) = sumChar >= (policy ^. #lower) && sumChar <= policy ^. #upper
      where
        sumChar = T.length $ T.filter (== policy ^. #char) password

partB :: [(Policy, Password)] -> Int
partB = getSum . foldMap (Sum . fromEnum . satisfies)
  where
    satisfies (policy, password) = F.elem True $ liftA2 xor lwr upr
      where
        lwr = password ^? ix (policy ^. #lower - 1)
        upr = password ^? ix (policy ^. #upper - 1)
        xor l u = (l == policy ^. #char || u == policy ^. #char) && u /= l
