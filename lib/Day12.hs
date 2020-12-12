{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day12 where

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
import qualified Debug.Trace as Debug
import Data.Functor.Compose (Compose(Compose))

data Command = Nor Int
             | Sou Int
             | Eas Int
             | Wes Int
             | Lef Int
             | Rig Int
             | For Int
             deriving (Eq, Show, Generic)

data Heading = N | E | S | W deriving (Eq, Show, Generic, Ord, Enum)

rotLeft :: Int -> Heading -> Heading
rotLeft n = toEnum . (`mod` 4) . subtract n . fromEnum

rotRight :: Int -> Heading -> Heading
rotRight n = toEnum . (`mod` 4) . (+n) . fromEnum

parseRotation :: Atto.Parser Int
parseRotation = (`div` 90) <$> Atto.decimal

parseCommand :: Atto.Parser Command
parseCommand = Nor <$> (Atto.char 'N' *> Atto.decimal)
           <|> Sou <$> (Atto.char 'S' *> Atto.decimal)
           <|> Eas <$> (Atto.char 'E' *> Atto.decimal)
           <|> Wes <$> (Atto.char 'W' *> Atto.decimal)
           <|> Lef <$> (Atto.char 'L' *> parseRotation)
           <|> Rig <$> (Atto.char 'R' *> parseRotation)
           <|> For <$> (Atto.char 'F' *> Atto.decimal)

parseFile :: Atto.Parser (Seq.Seq Command)
parseFile = Seq.fromList <$> parseCommand `Atto.sepBy'` Atto.endOfLine

readInput :: FilePath -> IO (Seq.Seq Command)
readInput = either fail pure
          . Atto.parseOnly parseFile
          <=< Text.readFile

data Ship = Ship
  { position :: (Int, Int) -- x,y
  , heading :: Heading
  } deriving (Eq, Show, Generic)

initShip :: Ship
initShip = Ship (0,0) E

moveHeading :: Int -> Ship -> Ship
moveHeading n ship = ship & #position %~ delta
  where
    delta = case ship ^. #heading of
      N -> _2 +~ n
      S -> _2 -~ n
      E -> _1 +~ n
      W -> _1 -~ n

step :: Command -> Ship -> Ship
step (Nor n) = #position . _2 +~ n
step (Sou n) = #position . _2 -~ n
step (Eas n) = #position . _1 +~ n
step (Wes n) = #position . _1 -~ n
step (Lef n) = #heading %~ rotLeft n
step (Rig n) = #heading %~ rotRight n
step (For n) = moveHeading n

manhattan :: (Int, Int) -> Int
manhattan (x,y) = abs x + abs y

_dual :: Iso' (Dual a) a
_dual = _Wrapped

_endo :: Iso' (Endo a) (a->a)
_endo = _Wrapped

partA :: Seq.Seq Command -> Int
partA = manhattan . view #position . ($ initShip) . auf (_dual._endo) foldMap step

data Ship' = Ship'
  { position :: (Int, Int) -- x,y
  , waypoint :: (Int, Int) -- x,y
  } deriving (Eq, Show, Generic)

initShip' :: Ship'
initShip' = Ship' (0,0) (10,1)

rotLeft' :: Int -> (Int, Int) -> (Int, Int)
rotLeft' 0 (ew,ns) = (ew,ns)
rotLeft' n (ew,ns) = rotLeft' (n-1) (-ns, ew)

rotRight' :: Int -> (Int, Int) -> (Int, Int)
rotRight' 0 (ew,ns) = (ew,ns)
rotRight' n (ew,ns) = rotRight' (n-1) (ns,-ew)

moveHeading' :: Int -> Ship' -> Ship'
moveHeading' n ship = ship & #position %~ delta
  where
    delta (x,y) = (x + dx, y + dy)
      where
        (dx,dy) = over both (*n) (ship ^. #waypoint)

step' :: Command -> Ship' -> Ship'
step' (Nor n) = #waypoint . _2 +~ n
step' (Sou n) = #waypoint . _2 -~ n
step' (Eas n) = #waypoint . _1 +~ n
step' (Wes n) = #waypoint . _1 -~ n
step' (Lef n) = #waypoint %~ rotLeft' n
step' (Rig n) = #waypoint %~ rotRight' n
step' (For n) = moveHeading' n

partB :: Seq.Seq Command -> Int
partB = manhattan . view #position . ($ initShip') . auf (_dual._endo) foldMap step'
