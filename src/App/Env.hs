module App.Env where

import Data.IORef
import Data.Map qualified as M
import Data.Text (Text)


import App.Config
import Bot.Types

import FrontEnd.FrontEnd
import FrontEnd.Web

import Logger.Handle qualified as Logger
import Logger.IO qualified as Logger

data Env f = Env
    { envLogger         :: Logger.Logger IO
    , envDefaultRepeats :: !Repeat    
    , envHelpMessage    :: !Text
    , envRepeatMessage  :: !Text
    , envReps           :: IORef (M.Map (User f) Repeat)
    , envToken          :: WebOnly f (Token f)
    , envFrontData      :: IORef (FrontData f)
    , envPollingTime    :: WebOnly f PollingTime
    }

newEnv :: forall f. IsFrontEnd f
    => Config f -> IO (Env f)
newEnv Config{..} = do
    envReps <- newIORef M.empty  
    envFrontData <- newFrontData @f @IO cToken >>= newIORef 
    let envLogger         = Logger.fromConfig cLogger
        envDefaultRepeats = cDefaultRepeats
        envHelpMessage    = cHelpMessage
        envRepeatMessage  = cRepeatMessage
        envToken          = cToken
        envPollingTime    = cPollingTime
    pure $ Env{..}