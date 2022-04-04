{-# LANGUAGE DeriveAnyClass #-}

module Bot.Error where

import Control.Exception ( Exception )
import Control.Monad.Catch (MonadThrow(..), handle, MonadCatch ) 
import Data.Aeson ( FromJSON, eitherDecode )
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Extended.Text (readEither)

data BotError
    = ParsingError Text
    | BadCallbackError Text
    | ImpossibleHappened Text
    deriving (Show, Exception, Eq)

parse :: (FromJSON x, MonadThrow m) => BSL.ByteString -> m x
parse = either (throwM . ParsingError . T.pack) pure . eitherDecode 

parseCatch :: (FromJSON a, MonadCatch m) => String -> BSL.ByteString -> m a
parseCatch err = handle (\(ParsingError _) -> throwM $ ParsingError $ T.pack err) . parse

readB :: (Read a, Monad m, MonadThrow m) => String -> m Int
readB = either (throwM . ParsingError . T.pack) pure . readEither 

impossibleHappened :: MonadThrow m => Text -> m a
impossibleHappened = throwM . ImpossibleHappened