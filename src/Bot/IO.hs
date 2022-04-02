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

class FrontEndIO f (m :: Type -> Type) where

    getUpdates :: m [Update f]

    sendResponse :: Text -> m ()

    sendWebResponse :: WebOnly f Text -> m ()

-- instance {-# OVERLAPPING #-}
--     ( Monad m
--     , Token f ~ NotRequired 
--     ) => FrontEndIO f m where

instance {-# OVERLAPPING #-}
    ( Monad m
    , MonadThrow m
    , HTTP.MonadHttp m
    , Logger.HasLogger m
    , MonadCatch m
    , IsWebFrontEnd f m
    , FromJSON (Response f)
    , FromJSON (BadResponse f)
    , Show (Response f)
    ) 
    => FrontEndIO f m where

    getUpdates = 
        getUpdatesURL @f @m <$> getToken <*> getFrontData <*> getPollingTime 
        >>= HTTP.tryRequest 
        >>= \x -> case eitherDecode @(Response f) x of
            Left err -> parseCatch @(BadResponse f) err x >>= fmap (fmap (const [])) (handleBadResponse @f)
            Right r -> do
                Logger.debug $ "Recieved response: " Logger..< r
                updateFrontData $ extractFrontData @f @m r 
                pure $ extractUpdates @f @m r

    sendResponse = HTTP.tryRequest >=> checkCallback 

    sendWebResponse = sendResponse @f @m