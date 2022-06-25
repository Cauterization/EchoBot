{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.Types where

import Control.Monad (unless)
import Data.Aeson (FromJSON (parseJSON))
import Dhall (FromDhall, Generic, Text)
import Test.QuickCheck (Arbitrary)

newtype ID e = ID {idVal :: Int}
  deriving newtype (Show, Eq, Ord, Read, Enum, Num, FromJSON, Arbitrary, FromDhall)

newtype Repeat = Repeat {unRepeat :: Int}
  deriving (Generic, Eq, Ord)
  deriving newtype (Arbitrary, Num, Read, Show, FromDhall)

instance FromJSON Repeat where
  parseJSON r = do
    x <- parseJSON @Int r
    unless (x `elem` [1 .. 5]) $ fail "incorrect number of repeatitions"
    pure $ Repeat x

type URL = Text

newtype Token = Token {unToken :: Text}
  deriving (Show)
  deriving newtype (FromDhall)

type PollingTime = Int
