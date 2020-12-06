{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Day6 where

import qualified Data.Text.IO as Text

import qualified Data.Attoparsec.Text as Atto

import Control.Monad ((<=<))

import qualified Data.Foldable as F
import qualified Data.Set as Set

type Survey = Set.Set Char
type SurveyGroup = [Survey]

parseSurvey :: Atto.Parser Survey
parseSurvey = Set.fromList <$> Atto.many1' Atto.letter

parseGroup :: Atto.Parser SurveyGroup
parseGroup = parseSurvey `Atto.sepBy1'` Atto.endOfLine

parseFile :: Atto.Parser [SurveyGroup]
parseFile = parseGroup `Atto.sepBy1'` Atto.string "\n\n"

readInput :: FilePath -> IO [SurveyGroup]
readInput = either fail pure
          . Atto.parseOnly parseFile
          <=< Text.readFile

partA :: [SurveyGroup] -> Int
partA = sum . fmap positives
  where
    positives :: SurveyGroup -> Int
    positives = Set.size . F.fold

partB :: [SurveyGroup] -> Int
partB = sum . fmap positives
  where
    positives :: SurveyGroup -> Int
    positives = Set.size . F.foldl1 Set.intersection
