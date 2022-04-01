module Console.FrontEnd where

import Extended.Text (Text)
import Extended.Text qualified as T

newtype ConsoleText = ConsoleText {unCT :: Text}

getLine :: IO ConsoleText
getLine = ConsoleText <$> T.getLine