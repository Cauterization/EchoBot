module Bot.Bot where

import Control.Monad.Reader
import Bot.FrontEnd
import Bot.IO
import Logger.Handle ((.<))
import Logger.Handle qualified as Logger

bot :: forall f m a. IsBot f m => m a
bot = forever $ {-handle handler $-} do
    Logger.info "Getting updates.."
    updates <- getUpdates @f
    Logger.info $ "Recieved " .< length updates <> " new updates."
    actions <- concat <$> mapM getActions updates
    forM_ actions $ executeAction @f

executeAction :: forall f m. IsBot f m => Action f -> m ()
executeAction = \case
    UpdateRepeats u r      -> setRepeats u r 
    HideKeyboard hk        -> sendWebResponse @f hk 
    SendEcho echo          -> sendResponse @f echo
    SendHelpMessage echo   -> sendResponse @f echo
    SendRepeatMessage echo -> sendResponse @f echo
    SendRepeatEcho user se -> getRepeatsFor user 
        >>= flip replicateM_ (sendResponse @f se)

-- handler = undefined

