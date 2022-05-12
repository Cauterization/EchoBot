module App.Env where

import App.Config ( Config(..) )
import Bot.FrontEnd
    ( Token, IsFrontEnd(BotUser, FrontData, WebOnly, newFrontData) )
import Bot.Types ( Repeat, PollingTime )
import Data.IORef ( IORef, newIORef )
import Data.Map qualified as M
import Data.Text (Text)
import Logger.Handle qualified as Logger
import Logger.IO qualified as Logger

data Env f = Env
  { envLogger :: !(Logger.Logger IO),
    envDefaultRepeats :: !Repeat,
    envHelpMessage :: !Text,
    envRepeatMessage :: !Text,
    envRepeats :: !(IORef (M.Map (BotUser f) Repeat)),
    envToken :: !(WebOnly f (Token f)),
    envFrontData :: !(IORef (FrontData f)),
    envPollingTime :: !(WebOnly f PollingTime)
  }

newEnv :: forall f. IsFrontEnd f => Config f -> IO (Env f)
newEnv Config {..} = do
  envRepeats <- newIORef M.empty
  envFrontData <- newFrontData @f cToken >>= newIORef
  let envLogger = Logger.fromConfig cLogger
      envDefaultRepeats = cDefaultRepeats
      envHelpMessage = cHelpMessage
      envRepeatMessage = cRepeatMessage
      envToken = cToken
      envPollingTime = cPollingTime
  pure $ Env {..}
