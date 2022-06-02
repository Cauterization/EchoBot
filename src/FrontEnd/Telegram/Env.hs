{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module FrontEnd.Telegram.Env where

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
import Bot.Types (PollingTime, Token)
import Control.Lens (makeLenses)
import Control.Monad.Catch (MonadThrow)
import Dhall (FromDhall, Generic)
import FrontEnd.Telegram.Config (TGConfig (cToken))
import Logger qualified

data TGEnv = TGEnv
  { _envOffset :: Int,
    envToken :: Token,
    envPollingTime :: PollingTime
  }
  deriving (Generic, FromDhall, Show)

makeLenses ''TGEnv

mkTGEnv :: (Monad m, MonadThrow m, Logger.HasLogger m) => App.Config -> m TGEnv
mkTGEnv App.Config {..} = case (cToken <$> cTGConfig, cPollingTime) of
  (Nothing, _) -> missingFieldError "Vk token or whole vk config file"
  (_, Nothing) -> missingFieldError "Polling time"
  (Just t, Just p) ->
    pure $
      TGEnv
        { _envOffset = 0,
          envToken = t,
          envPollingTime = p
        }
