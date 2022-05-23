module App.Config where

import Bot.Types (PollingTime, Repeat)
import Control.Monad.Catch (Exception, MonadThrow (..))
import Dhall (FromDhall, Generic, Text, auto, input)
import Extended.Text qualified as T
import FrontEnd.Telegram.Config qualified as TG
import FrontEnd.Vkontakte.Config qualified as VK
import Logger qualified

data ConfigFront = Vkontakte | Telegram | Console
  deriving (Show, Generic, FromDhall)

data Config = Config
  { cLogger :: !Logger.Config,
    cDefaultRepeats :: !Repeat,
    cHelpMessage :: !Text,
    cRepeatMessage :: !Text,
    cFrontEnd :: !ConfigFront,
    cPollingTime :: !(Maybe PollingTime),
    cVKConfig :: !(Maybe VK.VKConfig),
    cTGConfig :: !(Maybe TG.TGConfig)
  }
  deriving (Show, Generic, FromDhall)

newtype ConfigError = ConfigError Text deriving (Show, Exception)

getConfig :: FilePath -> IO Config
getConfig = input auto . T.pack

missingFieldError :: (Monad m, MonadThrow m, Logger.HasLogger m) => Text -> m a
missingFieldError f = do
  let err = "Some mandatory field(" <> f <> ") is missing in configuration file."
  Logger.error err
  throwM $ ConfigError err
