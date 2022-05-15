module Bot.IO where

import Bot.Error ( parseCatch )
import Bot.FrontEnd
    ( HasEnv(getPollingTime, getToken, getFrontData),
      IsWebFrontEnd(..),
      IsFrontEnd(WebOnly, Update),
      updateFrontData )
import Control.Monad ((>=>))
import Control.Monad.Catch ( MonadCatch )
import Data.Aeson ( eitherDecode, FromJSON )
import Data.Kind (Type)
import Extended.HTTP qualified as HTTP
import Extended.Text (Text)
import Logger.Handle qualified as Logger

-- | Class for bot input and output
-- note that it have one common instance for all messangers and another for console
class FrontEndIO f (m :: Type -> Type) where
  getUpdates :: m [Update f]

  sendResponse :: Text -> m ()

  sendWebResponse :: WebOnly f Text -> m ()

type IsBot f m =
  ( Monad m,
    MonadCatch m,
    Logger.HasLogger m,
    IsFrontEnd f,
    FrontEndIO f m,
    HasEnv f m
  )

instance
  {-# OVERLAPS #-}
  ( IsBot f m,
    HTTP.MonadHttp m,
    IsWebFrontEnd m f,
    FromJSON (Response f),
    FromJSON (BadResponse f),
    Show (Response f)
  ) =>
  FrontEndIO f m
  where
  getUpdates =
    getUpdatesURL @m <$> getToken <*> getFrontData <*> getPollingTime
      >>= HTTP.tryRequest
      >>= \x -> case eitherDecode @(Response f) x of
        Left err ->
          parseCatch @(BadResponse f) err x
            >>= fmap (fmap (const [])) handleBadResponse
        Right r -> do
          Logger.debug $ "Recieved response: " Logger..< r
          updateFrontData $ extractFrontData @m r
          pure $ extractUpdates @m r

  sendResponse = HTTP.tryRequest >=> checkCallback

  sendWebResponse = sendResponse @f
