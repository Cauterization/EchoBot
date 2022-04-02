{-# LANGUAGE DeriveAnyClass #-}

module Bot.Error where

import Control.Exception ( Exception )

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Control.Monad.Catch 
import Data.ByteString.Lazy qualified as BSL
import Extended.Text (readEither)

data BotError
    = ParsingError Text
    | Restart
    deriving (Show, Exception)

parse :: (FromJSON x, MonadThrow m) => BSL.ByteString -> m x
parse = either (throwM . ParsingError . T.pack) pure . eitherDecode 

parseCatch :: (FromJSON a, MonadCatch m) => String -> BSL.ByteString -> m a
parseCatch err = handle (\(e :: BotError) -> throwM $ ParsingError $ T.pack err) . parse

readB :: (Read a, Monad m, MonadThrow m) => String -> m Int
readB = either (throwM . ParsingError . T.pack) pure . readEither 
