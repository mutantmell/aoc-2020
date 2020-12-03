module Common where

import Data.Text (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

parseLines :: FilePath -> IO [Text]
parseLines file = Text.lines <$> Text.readFile file

dup :: a -> (a, a)
dup x = (x,x)