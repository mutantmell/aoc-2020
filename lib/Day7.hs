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
import Data.Maybe (maybeToList)
import Data.Monoid (getSum, Sum(Sum))
import qualified Data.Set as Set

type Bag = (Text, Text)
data BagRule = BagRule
  { bag :: Bag
  , canContain :: [(Int, Bag)]
  } deriving (Eq, Show, Generic)

parseBag :: Atto.Parser Bag
parseBag = (,) <$> Atto.takeWhile (not . Char.isSpace) 
               <* Atto.char ' '
               <*> Atto.takeWhile (not . Char.isSpace)
               <* Atto.char ' '
               <* Atto.string "bag"
               <* optional (Atto.char 's')

parseContains :: Atto.Parser (Int, Bag)
parseContains = (,) <$> Atto.decimal <* Atto.char ' ' <*> parseBag

parseBagRule :: Atto.Parser BagRule
parseBagRule = BagRule <$> parseBag
                       <* Atto.string " contain "
                       <*> (
                           parseContains `Atto.sepBy1` Atto.string ", "
                           <|> Atto.string "no other bags" $> []
                           )
                       <* Atto.char '.'
                        

parseFile :: Atto.Parser [BagRule]
parseFile = parseBagRule `Atto.sepBy1'` Atto.endOfLine

readInput :: FilePath -> IO [BagRule]
readInput = either fail pure
          . Atto.parseOnly parseFile
          <=< Text.readFile

closure :: [BagRule] -> Bag -> [Bag]
closure brs initB = go [] [initB]
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
    sub label = brs ^.. folded . filteredBy (#canContain . traverse . _2 . only label) . #bag

partAClosure :: [BagRule] -> [Bag]
partAClosure brs = closure brs ("shiny", "gold")

partAClosure' :: [BagRule] -> Tree.Tree Bag
partAClosure' = closure' ("shiny", "gold")

partA :: [BagRule] -> Int
partA = subtract 1 . F.length . partAClosure

partA' :: [BagRule] -> Int
partA' = subtract 1 . Set.size . foldMap Set.singleton . partAClosure'

asTree :: Bag -> [BagRule] -> Tree.Tree (Int, Bag)
asTree root brs = Tree.unfoldTree f (1, root)
  where
    f :: (Int, Bag) -> ((Int, Bag), [(Int, Bag)])
    f node@(num, label) = (node, over traverse (_1 *~ num) (sub label))
    sub label = maybeToList (F.find (\br -> br ^. #bag == label) brs) >>= (^. #canContain)

partBClosure :: [BagRule] -> Tree.Tree (Int, Bag)
partBClosure = asTree ("shiny", "gold")

partB :: [BagRule] -> Int
partB = subtract 1 . alaf Sum foldMap fst . partBClosure
