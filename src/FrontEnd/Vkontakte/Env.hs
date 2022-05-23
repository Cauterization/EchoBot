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
import FrontEnd.Vkontakte.Config (VKConfig (cToken))
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
    envPollingTime :: !PollingTime
  }
  deriving (Show)

makeLenses ''VKEnv

mkVkEnv :: (Monad m, MonadThrow m, HTTP.MonadHttp m, Logger.HasLogger m, MonadWait m) => App.Config -> m VKEnv
mkVkEnv App.Config {..} = case (cToken <$> cVKConfig, cPollingTime) of
  (Nothing, _) -> missingFieldError "Whole vk config file"
  (_, Nothing) -> missingFieldError "Polling time"
  (Just t, Just p) -> toEnv t p <$> getReponseWithFrontData t

getReponseWithFrontData :: (Monad m, HTTP.MonadHttp m, Logger.HasLogger m, MonadWait m) => Token -> m FrontDataResponse
getReponseWithFrontData (Token t) = do
  response <- HTTP.tryRequest req
  either (const $ handleFrontDataError response) pure $ eitherDecode response
  where
    req =
      "https://api.vk.com/method/groups.getLongPollServer"
        <> "?group_id=204518764"
        <> "&access_token="
        <> t
        <> "&v=5.81"
    failMsg = "An unknown error occurred while receiving Vkontakte's front end data. Retry after 30 sec."
    handleFrontDataError response = do
      Logger.error $ either (const failMsg) unVkMkEnvError $ eitherDecode response
      wait 30
      getReponseWithFrontData (Token t)

toEnv :: Token -> PollingTime -> FrontDataResponse -> VKEnv
toEnv t p FrontDataResponse {..} =
  VKEnv
    { envToken = t,
      _envTs = ts,
      _envKey = key,
      _envServer = server,
      envPollingTime = p
    }

newtype VkMkEnvError = VkMkEnvError {unVkMkEnvError :: Text}

instance FromJSON VkMkEnvError where
  parseJSON = withObject "FrontDataError" $ \o -> do
    o .: "error" >>= (.: "error_msg") <&> VkMkEnvError
