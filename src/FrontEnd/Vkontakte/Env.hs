{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module FrontEnd.Vkontakte.Env where

import App.Config
  ( Config
      ( cDefaultRepeats,
        cFrontEnd,
        cHelpMessage,
        cLogger,
        cPollingTime,
        cRepeatMessage,
        cTGConfig,
        cVKConfig
      ),
    missingFieldError,
  )
import App.Config qualified as App
import Bot.Types
import Control.Lens
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:))
import Extended.HTTP qualified as HTTP
import Extended.Text (Text)
import Extended.Text qualified as T
import FrontEnd.Vkontakte.Config (VKConfig (..), VKGroup)
import FrontEnd.Vkontakte.Internal
  ( FrontDataResponse (..),
    Key,
    Server,
    Ts,
  )
import Logger qualified
import Wait (MonadWait (..))

data VKEnv = VKEnv
  { envToken :: !Token,
    _envTs :: !Ts,
    _envKey :: !Key,
    _envServer :: !Server,
    envPollingTime :: !PollingTime,
    envGroupID :: !(ID VKGroup)
  }
  deriving (Show)

makeLenses ''VKEnv

mkVkEnv :: (Monad m, MonadThrow m, HTTP.MonadHttp m, Logger.HasLogger m, MonadWait m) => App.Config -> m VKEnv
mkVkEnv App.Config {..} = case (cVKConfig, cPollingTime) of
  (Nothing, _) -> missingFieldError "Whole vk config file"
  (_, Nothing) -> missingFieldError "Polling time"
  (Just VKConfig {..}, Just p) -> toEnv VKConfig {..} p <$> getReponseWithFrontData cToken cGroupID p

getReponseWithFrontData ::
  (Monad m, HTTP.MonadHttp m, Logger.HasLogger m, MonadWait m) =>
  Token ->
  ID VKGroup ->
  PollingTime ->
  m FrontDataResponse
getReponseWithFrontData (Token t) (ID groupID) pollingTime = do
  response <- HTTP.tryRequest pollingTime req
  either (const $ handleFrontDataError response) pure $ eitherDecode response
  where
    req =
      "https://api.vk.com/method/groups.getLongPollServer"
        <> "?group_id="
        <> T.show groupID
        <> "&access_token="
        <> t
        <> "&v=5.81"
    failMsg = "An unknown error occurred while receiving Vkontakte's front end data. Retry after 30 sec."
    handleFrontDataError response = do
      Logger.error $ either (const failMsg) unVkMkEnvError $ eitherDecode response
      wait 30
      getReponseWithFrontData (Token t) (ID groupID) pollingTime

toEnv :: VKConfig -> PollingTime -> FrontDataResponse -> VKEnv
toEnv VKConfig {..} p FrontDataResponse {..} =
  VKEnv
    { envToken = cToken,
      _envTs = ts,
      _envKey = key,
      _envServer = server,
      envPollingTime = p,
      envGroupID = cGroupID
    }

newtype VkMkEnvError = VkMkEnvError {unVkMkEnvError :: Text}

instance FromJSON VkMkEnvError where
  parseJSON = withObject "FrontDataError" $ \o -> do
    o .: "error" >>= (.: "error_msg") <&> VkMkEnvError
