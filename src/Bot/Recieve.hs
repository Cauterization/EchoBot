{-# LANGUAGE FunctionalDependencies #-}
module Bot.Recieve where

import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Aeson
import Data.Functor ((<&>))

import Extended.Text qualified as T

import Extended.HTTP qualified as HTTP

import FrontEnd.FrontEnd

import Bot.Error
import Bot.Types

class IsFrontEnd f => CanRecieveUpdates (f :: FrontEnd) (m :: * -> *) | m -> f where
    recieve :: m [Update f]

instance MonadIO m => CanRecieveUpdates 'Console m where
    recieve = liftIO T.getLine <&> extractUpdates

instance {-# OVERLAPPABLE #-}
    ( IsFrontEnd f
    , Monad m
    , HTTP.MonadHttp m
    , MonadThrow m
    , HasEnv f m
    , WebOnly f URL ~ URL
    , WebOnly f (Token f) ~ Token f
    , WebOnly f (ConnectionData f) ~ ConnectionData f
    , WebOnly f PollingTime ~ PollingTime
    , FromJSON (Response f)
    ) 
    => CanRecieveUpdates (f :: FrontEnd) m where
    recieve = do
        token    <- getToken
        connData <- getConnData 
        polling  <- getPollingTime
        response <- HTTP.tryRequest (getUpdatesURL @f token connData polling)
        parse @(Response f) response >>= \case
            GoodResponse g -> pure $ extractUpdates g
            BadResponse b  -> throwM $ BadResponseErr @f b