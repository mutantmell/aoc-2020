module Common where

import Data.Text (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Control.Category (Category)
import Control.Lens (ala)
import Endo (Endo(Endo))
import qualified Data.Attoparsec.Text as Atto

parseLines :: FilePath -> IO [Text]
parseLines file = Text.lines <$> Text.readFile file

dup :: a -> (a, a)
dup x = (x,x)

{-# INLINE applyN #-}
applyN :: Int -> (a -> a) -> a -> a
applyN = applyN'

applyN' :: (Category c) => Int -> c a a -> c a a
applyN' n = ala Endo foldMap . replicate n

parseAll :: Atto.Parser a -> Text -> Either String a
parseAll p = Atto.parseOnly (p <* Atto.endOfInput)
