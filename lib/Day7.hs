{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Day7 where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Foldable as F
import qualified Data.Witherable as W

import Data.Generics.Labels ()

import           Data.Text (Text)
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import Control.Monad ((<=<))
import qualified Data.Char as Char
import Control.Applicative ((<|>), optional)
import Data.Functor (($>))

import qualified Data.Tree as Tree

import Control.Lens
import qualified Data.Set as Set
import Data.Monoid (Sum(Sum))
import qualified Data.Map as Map

type Bag = (Text, Text)
data BagRule = BagRule
  { bag :: Bag
  , canContain :: [(Int, Bag)]
  } deriving (Eq, Show, Generic)

type BagRules = Map.Map Bag [(Int, Bag)]

parseBag :: Atto.Parser Bag
parseBag = (,) <$> Atto.takeWhile (not . Char.isSpace) 
               <* Atto.char ' '
               <*> Atto.takeWhile (not . Char.isSpace)
               <* Atto.char ' '
               <* Atto.string "bag"
               <* optional (Atto.char 's')

parseContains :: Atto.Parser (Int, Bag)
parseContains = (,) <$> Atto.decimal <* Atto.char ' ' <*> parseBag

parseBagRule' :: (Bag -> [(Int, Bag)] -> a) -> Atto.Parser a
parseBagRule' f = f <$> parseBag
                    <* Atto.string " contain "
                    <*> (
                        parseContains `Atto.sepBy1` Atto.string ", "
                        <|> Atto.string "no other bags" $> []
                        )
                    <* Atto.char '.'

parseBagRule :: Atto.Parser BagRule
parseBagRule = parseBagRule' BagRule

parseFile :: Atto.Parser [BagRule]
parseFile = parseBagRule `Atto.sepBy1'` Atto.endOfLine

parseFile' :: Atto.Parser BagRules
parseFile' = Map.fromList <$> parseBagRule' (,) `Atto.sepBy1'` Atto.endOfLine

readInput :: FilePath -> IO [BagRule]
readInput = either fail pure
          . Atto.parseOnly parseFile
          <=< Text.readFile

readInput' :: FilePath -> IO BagRules
readInput' = either fail pure
          . Atto.parseOnly parseFile'
          <=< Text.readFile

closure :: Bag -> [BagRule] -> [Bag]
closure root brs = go [] [root]
  where
    go :: [Bag] -> [Bag] -> [Bag]
    go seen [] = seen
    go seen (b:bs) | b `elem` seen = go seen bs
    go seen (b:bs) = let
        canContain :: Bag -> [Bag]
        canContain b = W.mapMaybe (\br -> if
                b `elem` br ^.. #canContain . traverse . _2
            then Just (br ^. #bag)
            else Nothing) brs
        in
          go (b:seen) (bs ++ canContain b)

closure' :: Bag -> [BagRule] -> Tree.Tree Bag
closure' root brs = Tree.unfoldTree f root
  where
    f :: Bag -> (Bag, [Bag])
    f label = (label, sub label)
    --sub label = brs ^.. folded . filteredBy (#canContain . traverse . _2 . only label) . #bag
    sub label = brs ^.. folded . filtered (has $ #canContain . traverse . _2 . only label) . #bag

closure'' :: Bag -> BagRules -> Tree.Tree Bag
closure'' root brs = Tree.unfoldTree f root
  where
    f :: Bag -> (Bag, [Bag])
    f label = (label, sub label)
    sub label = brs ^.. itraversed . filtered (has $ traverse . _2 .only label) . asIndex

partA :: [BagRule] -> Int
partA = subtract 1 . F.length . closure ("shiny", "gold")

partA' :: [BagRule] -> Int
partA' = subtract 1 . Set.size . foldMap Set.singleton . closure' ("shiny", "gold")

partA'' :: BagRules -> Int
partA'' = subtract 1 . Set.size . foldMap Set.singleton . closure'' ("shiny", "gold")

singleton :: Prism' [a] a
singleton = prism' (:[]) (\case { [x] -> Just x; _ -> Nothing })

asTree :: Bag -> [BagRule] -> Tree.Tree (Int, Bag)
asTree root brs = Tree.unfoldTree f (1, root)
  where
    f :: (Int, Bag) -> ((Int, Bag), [(Int, Bag)])
    f node@(num, label) = (node, over traverse (_1 *~ num) (sub label))
    --sub label = brs ^.. folded . filteredBy (#bag . only label) . #canContain . traverse
    sub label = brs ^. to (filter $ has (#bag . only label)) . singleton . #canContain

asTree' :: Bag -> BagRules -> Tree.Tree (Int, Bag)
asTree' root brs = Tree.unfoldTree f (1, root)
  where
    f :: (Int, Bag) -> ((Int, Bag), [(Int, Bag)])
    f node@(num, label) = (node, over traverse (_1 *~ num) (sub label))
    sub label = brs ^. itraversed . indices (== label)

partB :: [BagRule] -> Int
partB = subtract 1 . alaf Sum foldMap fst . asTree ("shiny", "gold")

partB' :: BagRules -> Int
partB' = subtract 1 . alaf Sum foldMap fst . asTree' ("shiny", "gold")
