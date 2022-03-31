module Bot.Env where

import Data.IORef
import Data.Map qualified as M
import Data.Text (Text)

import FrontEnd.FrontEnd
import Bot.Types

import Logger.Handle qualified as Logger

data Env (f :: FrontEnd) m = Env
    { envLogger         :: Logger.Logger IO
    , envDefaultReplies :: !Int    
    , envHelpMes        :: !Text
    , envRepMes         :: !Text
    --, envReps           :: IORef (M.Map (User f) Rep)
    , envToken          :: WebField f (Token f)
    --, envConnData       :: WebField f (IORef (ConnData f))
    , envPollingTime    :: WebField f Int
    }