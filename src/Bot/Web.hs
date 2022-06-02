{-# LANGUAGE ImportQualifiedPost #-}

module Bot.Web where

import Bot.FrontEnd (HasEnv, IsFrontEnd (Update))
import Bot.Types (PollingTime, Token, URL)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.Kind (Type)
import Extended.HTTP qualified as HTTP
import Logger qualified
import Wait (MonadWait)

class
  ( IsFrontEnd f,
    FromJSON (Response f),
    FromJSON (BadResponse f)
  ) =>
  IsWebFrontEnd f
  where
  getToken :: (Monad m, HasEnv f m) => m Token

  getPollingTime :: (Monad m, HasEnv f m) => m PollingTime

  getUpdatesURL :: (Monad m, HasEnv f m) => m URL

  type Response f :: Type

  extractUpdates :: Response f -> [Update f]

  updateFrontEnv :: (HasEnv f m, MonadThrow m) => Response f -> m ()

  type BadResponse f :: Type

  handleBadResponse ::
    ( Monad m,
      MonadThrow m,
      HTTP.MonadHttp m,
      Logger.HasLogger m,
      MonadWait m,
      HasEnv f m
    ) =>
    BadResponse f ->
    m ()

  checkCallback :: (Monad m, Logger.HasLogger m, MonadThrow m) => BSL.ByteString -> m ()
