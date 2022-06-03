{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module App.Env where

import App.Config (Config (..))
import Bot.FrontEnd
  ( IsFrontEnd (BotFrontEnv, BotUser, mkFrontEnv),
  )
import Bot.Types (Repeat)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, newIORef)
import Data.Map qualified as M
import Extended.Text (Text)
import Logger qualified

data Env f = Env
  { envLogger :: !(Logger.Logger IO),
    envDefaultRepeats :: !Repeat,
    envHelpMessage :: !Text,
    envRepeatMessage :: !Text,
    envRepeats :: !(IORef (M.Map (BotUser f) Repeat)),
    envFront :: !(IORef (BotFrontEnv f))
  }

newEnv :: forall f. IsFrontEnd f => Config -> Logger.LoggerMIO (Env f)
newEnv c@Config {..} = do
  let envLogger = Logger.fromConfig cLogger
      envDefaultRepeats = cDefaultRepeats
      envHelpMessage = cHelpMessage
      envRepeatMessage = cRepeatMessage
  envRepeats <- liftIO $ newIORef M.empty
  envFront <- mkFrontEnv @f c >>= liftIO . newIORef
  pure $ Env {..}
