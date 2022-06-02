{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Extended.Text
  ( module Data.Text,
    module Data.Text.IO,
    module Data.Text.Encoding,
    show,
    read,
    readEither,
  )
where

import Control.Arrow (ArrowChoice (left))
import Data.Text
import Data.Text.Encoding
import Data.Text.IO
import Test.QuickCheck.Arbitrary.Generic (Arbitrary (arbitrary))
import Text.Read qualified as Read
import Prelude hiding (read, show)
import Prelude qualified

show :: Show a => a -> Text
show = pack . Prelude.show

read :: Read a => Text -> a
read = Prelude.read . unpack

readEither :: Read a => Text -> Either Text a
readEither = left pack . Read.readEither . unpack

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary
