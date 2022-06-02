module Mocks.Constants where

import Bot.Types (PollingTime, Token (..))
import Extended.Text (Text)
import FrontEnd.Console.Env (ConsoleEnv (..))
import FrontEnd.Telegram.Env (TGEnv (..))
import FrontEnd.Vkontakte.Env (VKEnv (..))

defaultToken :: Token
defaultToken = Token "defaultToken"

defaultPollingTime :: PollingTime
defaultPollingTime = 30

defaultHelpMessage :: Text
defaultHelpMessage = "HelpMessage"

defaultRepeatMessage :: Text
defaultRepeatMessage = "RepeatMessage"

defaultConsoleEnv :: ConsoleEnv
defaultConsoleEnv =
  ConsoleEnv
    { _consoleAwaitsRepUpdate = False
    }

defaultTGEnv :: TGEnv
defaultTGEnv =
  TGEnv
    { _envOffset = 0,
      envToken = defaultToken,
      envPollingTime = defaultPollingTime
    }

defaultVKEnv :: VKEnv
defaultVKEnv =
  VKEnv
    { envToken = defaultToken,
      _envTs = 0,
      _envKey = "default VK key",
      _envServer = "default VK server",
      envPollingTime = defaultPollingTime,
      envGroupID = 0
    }
