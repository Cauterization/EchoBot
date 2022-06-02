module Mocks.With where

import Bot.FrontEnd (BotFrontEnv, Update)
import Extended.Text (Text)
import Mocks.TestBot (BotState (..))

withUpdate :: Update f -> (BotState f -> BotState f)
withUpdate = withUpdates . pure

withUpdates :: [Update f] -> (BotState f -> BotState f)
withUpdates us BotState {..} = BotState {bUpdates = bUpdates <> us, ..}

withFrontData :: BotFrontEnv f -> (BotState f -> BotState f)
withFrontData f BotState {..} = BotState {bFrontData = f, ..}

withHelpText, withRepeatText :: Text -> (BotState f -> BotState f)
withHelpText help BotState {..} = BotState {bHelpMessage = help, ..}
withRepeatText repeat BotState {..} = BotState {bRepeatMessage = repeat, ..}

woLogging :: BotState f -> BotState f
woLogging BotState {..} = BotState {bIsLogerRequired = False, ..}
