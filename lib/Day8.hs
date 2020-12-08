{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day8 where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Foldable as F

import Data.Generics.Labels ()

import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import Control.Monad ((<=<))
import Control.Lens
import Control.Applicative
import Data.Functor (($>))

data Instr = Nop Int
           | Acc Int
           | Jump Int
           deriving (Eq, Show, Generic)

parseInstr :: Atto.Parser Instr
parseInstr = instr <* Atto.space <*> Atto.signed Atto.decimal
  where
    instr = Atto.string "nop" $> Nop
          <|> Atto.string "acc" $> Acc
          <|> Atto.string "jmp" $> Jump

parseFile :: Atto.Parser [Instr]
parseFile = parseInstr `Atto.sepBy'` Atto.endOfLine

readInput :: FilePath -> IO [Instr]
readInput = either fail pure
          . Atto.parseOnly parseFile
          <=< Text.readFile

data Computer = Computer
  { accumulator :: Int
  , instructions :: [Instr]
  , pc :: Int
  } deriving (Eq, Show, Generic)

initComputer :: [Instr] -> Computer
initComputer instrs = Computer 0 instrs 0

step :: Computer -> Computer
step c = case c ^?! #instructions . ix (c ^. #pc) of
  Nop _ -> c & #pc +~ 1
  Acc n -> c & #accumulator +~ n
             & #pc +~ 1
  Jump n -> c & #pc +~ n

data Result = Result
  { accumulator :: Int
  , success :: Bool
  } deriving (Eq, Show, Generic)

run :: Computer -> Result
run = go []
  where
    go seen = f . step
      where
        f c | c ^. #pc `elem` seen = Result (c ^. #accumulator) False
            | c ^. #pc >= F.length (c ^. #instructions) = Result (c ^. #accumulator) True
            | otherwise = go (c ^. #pc : seen) c

partA :: [Instr] -> Int
partA = view #accumulator . run . initComputer

fuzz :: [Instr] -> [[Instr]]
fuzz = snd . go []
  where
    go :: [[Instr]] -> [Instr] -> ([Instr], [[Instr]])
    go acc [] = ([], acc)
    go acc (i@(Acc _):is) = go acc is & _2 %~ fmap (i:) & _1 %~ (i:)
    go acc (i@(Nop n):is) = go acc is & \(is, iss) -> (i:is, (Jump n:is):fmap (i:) iss)
    go acc (i@(Jump n):is) = go acc is & \(is, iss) -> (i:is, (Nop n:is):fmap (i:) iss)

partB :: [Instr] -> Int
partB = maybe (-1) (view #accumulator) . F.find (view #success) . fmap (run . initComputer) . fuzz
