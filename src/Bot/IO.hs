module Bot.IO where

import Control.Monad ((>=>))
import Control.Monad.Catch


import Data.Aeson 
import Data.Kind (Type, Constraint)


import Extended.Text (Text)

import Extended.HTTP qualified as HTTP



import Bot.Error

import qualified Logger.Handle as Logger


import Bot.FrontEnd
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Concurrent (threadDelay)

class MonadWait m where
    wait :: Int -> m ()

instance MonadIO m => MonadWait m where
    wait = liftIO . threadDelay . (*1000000)


class FrontEndIO f (m :: Type -> Type) where

    getUpdates :: m [Update f]

    sendResponse :: Text -> m ()

    sendWebResponse :: WebOnly f Text -> m ()

instance {-# OVERLAPPING #-}
    ( Monad m
    , MonadThrow m
    , HTTP.MonadHttp m
    , Logger.HasLogger m
    , MonadCatch m
    , IsWebFrontEnd m f
    , FromJSON (Response f)
    , FromJSON (BadResponse f)
    , Show (Response f)
    ) 
    => FrontEndIO f m where

    getUpdates = 
        getUpdatesURL @m <$> getToken <*> getFrontData <*> getPollingTime 
        >>= HTTP.tryRequest 
        >>= \x -> case eitherDecode @(Response f) x of
            Left err -> parseCatch @(BadResponse f) err x 
                    >>= fmap (fmap (const [])) (handleBadResponse @m)
            Right r -> do
                Logger.debug $ "Recieved response: " Logger..< r
                updateFrontData $ extractFrontData @m r 
                pure $ extractUpdates @m r

    sendResponse = HTTP.tryRequest >=> checkCallback 

    sendWebResponse = sendResponse @f 