module Extended.Text
    ( module Data.Text
    , module Data.Text.IO
    , module Data.Text.Encoding
    , module Text.Read
    , show
    , read
    ) where

import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import Text.Read (readEither)
import Prelude hiding (show, read)
import Prelude qualified

show :: Show a => a -> Text
show = pack . Prelude.show

read :: Read a => Text -> a
read = Prelude.read . unpack
