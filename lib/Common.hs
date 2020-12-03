module Common where

import Data.Text (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Control.Category (Category)
import Control.Lens (ala)
import Endo (Endo(Endo))

parseLines :: FilePath -> IO [Text]
parseLines file = Text.lines <$> Text.readFile file

dup :: a -> (a, a)
dup x = (x,x)

applyN :: (Category c) => Int -> c a a -> c a a
applyN n = ala Endo foldMap . replicate n
