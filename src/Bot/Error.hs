{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Bot.Error where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T

data BotError
  = ParsingError Text
  | BadCallbackError Text
  deriving (Show, Exception, Eq)

parse :: (FromJSON x, MonadThrow m) => BSL.ByteString -> m x
parse = either (parsingError . T.pack) pure . eitherDecode

parsingError :: MonadThrow m => Text -> m x
parsingError = throwM . ParsingError
