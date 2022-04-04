module Extended.Text
    ( module Data.Text
    , module Data.Text.IO
    , module Data.Text.Encoding
    , show
    , read
    , readEither
    ) where

import Control.Arrow

import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import Text.Read qualified as Read
import Prelude hiding (show, read)
import Prelude qualified


show :: Show a => a -> Text
show = pack . Prelude.show

read :: Read a => Text -> a
read = Prelude.read . unpack

readEither :: Read a => Text -> Either Text a
readEither = left pack . Read.readEither . unpack