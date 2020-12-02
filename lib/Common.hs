module Common where

import Data.Text (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

parseFile :: FilePath -> IO [Text]
parseFile file = Text.lines <$> Text.readFile file