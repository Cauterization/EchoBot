module Bot.IO where

import Bot.Error (BotError (ParsingError))
import Bot.FrontEnd (HasEnv, IsFrontEnd (Update))
import Bot.Web
  ( IsWebFrontEnd
      ( Response,
        checkCallback,
        extractUpdates,
        getUpdatesURL,
        handleBadResponse,
        updateFrontEnv
      ),
    getPollingTime,
  )
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Data.Aeson (eitherDecode)
import Data.Kind (Type)
import Extended.HTTP qualified as HTTP
import Extended.Text (Text)
import Extended.Text qualified as T
import Logger qualified
import Wait (MonadWait (..))

-- | Class for bot input and output
-- note that it have one common instance for all messangers and another for console
class FrontEndIO f (m :: Type -> Type) where
  getUpdates :: Monad m => m [Update f]
  default getUpdates ::
    ( MonadBot f m,
      IsWebFrontEnd f,
      HTTP.MonadHttp m,
      Logger.HasLogger m,
      Show (Response f)
    ) =>
    m [Update f]
  getUpdates = do
    response <- getResponse
    Logger.debug $ "Recieved response: " Logger..< response
    updateFrontEnv @f response
    pure $ extractUpdates @f response

  sendResponse :: Text -> m ()
  default sendResponse :: (MonadBot f m, HTTP.MonadHttp m, IsWebFrontEnd f) => Text -> m ()
  sendResponse resp = do
    pollingTime <- getPollingTime @f
    HTTP.tryRequest pollingTime resp >>= checkCallback @f

type MonadBot f m =
  ( Monad m,
    MonadCatch m,
    Logger.HasLogger m,
    MonadWait m,
    IsFrontEnd f,
    FrontEndIO f m,
    HasEnv f m
  )

getResponse :: forall f m. (IsWebFrontEnd f, MonadBot f m, HTTP.MonadHttp m) => m (Response f)
getResponse = do
  pollingTime <- getPollingTime @f
  response <- HTTP.tryRequest pollingTime =<< getUpdatesURL @f
  case eitherDecode response of
    Right decoded -> pure decoded
    Left err -> case eitherDecode response of
      Right badResponse -> handleBadResponse badResponse >> wait 30 >> getResponse
      _ -> throwM $ ParsingError $ T.pack err
