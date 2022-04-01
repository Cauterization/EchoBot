{-# LANGUAGE FunctionalDependencies #-}
module Bot.Recieve where

import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Aeson
import Data.Functor ((<&>))

import Extended.Text qualified as T

import Extended.HTTP qualified as HTTP

import FrontEnd.FrontEnd
import Console.FrontEnd qualified as Console

import Bot.Error
import Bot.Types

class FrontEndIO (f :: FrontEnd) (m :: * -> *) where
    mkRequest :: m (Response f)

instance MonadIO m => FrontEndIO 'Console m where
    mkRequest = liftIO T.getLine

class ( IsWebFrontEnd f 
      ) => HasWebEnv (f :: FrontEnd) m | m -> f where
    getConnData    :: m (ConnectionData f)
    setConnData    :: ConnectionData f -> m ()
    getToken       :: m (Token f)
    getPollingTime :: m PollingTime

instance {-# OVERLAPPABLE #-}
    ( IsFrontEnd f
    , Monad m
    , HTTP.MonadHttp m
    , MonadThrow m
    , HasWebEnv f m
    , FromJSON (Response f)
    ) 
    => FrontEndIO (f :: FrontEnd) m where
    mkRequest = getUpdatesURL @f <$> getToken <*> getConnData <*> getPollingTime 
        >>= HTTP.tryRequest >>= parse

