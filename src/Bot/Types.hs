{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.Types where

import Control.Monad (unless)

import Data.Aeson ( FromJSON(parseJSON) ) 

import Extended.Text (Text)
import Extended.Text qualified as T

import GHC.Generics (Generic)

import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary.Generic

newtype ID e = ID { idVal :: Int }
  deriving newtype (Show, Eq, Ord, Read, Enum, Num, FromJSON, Arbitrary)

newtype Repeat = Repeat {unRepeat :: Int} 
    deriving (Generic, Show, Eq, Ord)
    deriving newtype (Arbitrary, Num, Read)

instance FromJSON Repeat where
    parseJSON r = do
        x <- parseJSON @Int r
        unless (x `elem` [1..5]) $ fail "incorrect number of repeatitions"
        pure $ Repeat x

type URL = Text

type PollingTime = Int



-- DEBUG
instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary