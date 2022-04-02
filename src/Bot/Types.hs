{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.Types where

import Control.Applicative ((<|>), Alternative)
import Control.Monad (guard, unless)

import Data.Aeson (FromJSON, parseJSON, withText, withObject, Value (Object, String), (.:), eitherDecode)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))

import Extended.Text (Text)
import Extended.Text qualified as T

import Data.Typeable (Typeable, typeOf, Proxy (Proxy))

import GHC.Generics (Generic)

newtype ID e = ID { idVal :: Int }
  deriving newtype (Show, Eq, Ord, Read, Enum, FromJSON)

newtype Repeat = Repeat Int deriving (Generic, Show)

instance FromJSON Repeat where
    parseJSON r = do
        x <- parseJSON @Int r
        unless (x `elem` [1..5]) $ fail "incorrect number of default repeatitions"
        pure $ Repeat x

type URL = Text

type PollingTime = Int

