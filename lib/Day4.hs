{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}

module Day4 where

import GHC.Generics (Generic)
import Data.Generics.Labels ()

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import Control.Monad ((<=<))
import Control.Lens ((^?), ix, filtered, to, _Right)
import Control.Applicative (Applicative(liftA2), (<|>))
import Data.Functor (($>))
import qualified Data.Text as Text

import qualified Data.Witherable as W
import Common (parseAll)

parseField :: Atto.Parser (Text, Text)
parseField = (,) <$> Atto.take 3 <* Atto.char ':' <*> Atto.takeWhile (not . Char.isSpace)

parsePassport :: Atto.Parser (Map.Map Text Text)
parsePassport = Map.fromList <$> parseField `Atto.sepBy'` Atto.space

parseFile :: Atto.Parser [Map.Map Text Text]
parseFile = parsePassport `Atto.sepBy1'` Atto.string "\n\n"

readInput :: FilePath -> IO [Map.Map Text Text]
readInput = either fail pure
          . Atto.parseOnly parseFile
          <=< Text.readFile

validPassport :: Map.Map Text Text -> Bool
validPassport = and . traverse Map.member [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

partA :: [Map.Map Text Text] -> Int
partA = length . filter validPassport

data LengthUnits = In | CM deriving (Eq, Show, Generic)

data Passport = Passport
  { birthYear :: Int
  , issueYear :: Int
  , expirationYear :: Int
  , height :: (Int, LengthUnits)
  , hairColor :: Text
  , eyeColor :: Text
  , passportId :: Text
  , countryId :: Maybe Text
  } deriving (Eq, Show, Generic)

(<&&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<&&>) = liftA2 (&&)

parseYear :: Atto.Parser Int
parseYear = read <$> Atto.count 4 (Atto.satisfy Char.isNumber)

parseHeight :: Atto.Parser (Int, LengthUnits)
parseHeight = (,) <$> Atto.decimal <*> (Atto.string "cm" $> CM <|> Atto.string "in" $> In)

parseHair :: Atto.Parser Text
parseHair = (<>) <$> Atto.string "#" <*> (Text.pack <$> Atto.count 6 (Atto.satisfy (`elem` ['0'..'9'] ++ ['a'..'f'])))

parsePid :: Atto.Parser Text
parsePid = Text.pack <$> Atto.count 9 (Atto.satisfy Char.isDigit) -- this read is safe

fromMap :: Map.Map Text Text -> Maybe Passport -- make this an either?
fromMap map = do
  birthYear <- map ^? ix "byr" . to (parseAll parseYear) . _Right . filtered ((>= 1920) <&&> (<= 2002))
  issueYear <- map ^? ix "iyr" .  to (parseAll parseYear) . _Right . filtered ((>= 2010) <&&> (<= 2020))
  expirationYear <- map ^? ix "eyr" .  to (parseAll parseYear) . _Right . filtered ((>= 2020) <&&> (<= 2030))
  height <- map ^? ix "hgt" . to (parseAll parseHeight) . _Right . filtered \case
    (x, CM) -> 150 <= x && x <= 193
    (x, In) -> 59 <= x && x <= 76
  hairColor <- map ^? ix "hcl" . to (parseAll parseHair) . _Right
  eyeColor <- map ^? ix "ecl" . filtered (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
  passportId <- map ^? ix "pid" . to (parseAll parsePid) . _Right
  let countryId = map ^? ix "cid"
  pure Passport{..}

partB :: [Map.Map Text Text] -> Int
partB = length . W.mapMaybe fromMap
