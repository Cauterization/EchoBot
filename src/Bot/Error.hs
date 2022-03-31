{-# LANGUAGE DeriveAnyClass #-}

module Bot.Error where

import Control.Exception

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Control.Monad.Catch
import Data.ByteString.Lazy qualified as BSL

data Error =
    ParsingError Text
    deriving (Show, Exception)

parse :: (FromJSON x, MonadThrow a) => BSL.ByteString -> a x
parse = either (throwM . ParsingError . T.pack) pure . eitherDecode 
