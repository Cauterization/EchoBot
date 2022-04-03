module Bot.Bot where

import Control.Monad.Reader
import Bot.FrontEnd
import Bot.IO
import Bot.Types
import Logger.Handle ((.<))
import Logger.Handle qualified as Logger

bot :: forall f m a. IsBot f m => m a
bot = forever $ recieveActions @f >>= mapM_ (executeAction @f)

recieveActions :: forall f m. IsBot f m => m [Action f]
recieveActions = do
    Logger.info "Getting updates.."
    updates <- getUpdates @f
    Logger.info $ "Recieved " .< length updates <> " new updates."
    concat <$> mapM getActions updates

executeAction :: forall f m. IsBot f m => Action f -> m ()
executeAction = \case

    UpdateRepeats user rep -> do
        Logger.info $ "New number of repeations for user " .< user <> " is " .< rep
        setRepeats user rep

    HideKeyboard user url  -> do
        Logger.info $ "Hiding keyboard from " .< user
        sendWebResponse @f url

    SendEcho user url text  -> do
        Logger.info $ "Sending echo:\n" <> text <> "\nto: " .< user
        sendResponse @f url

    SendRepeatEcho  user url text -> do
        rep <- getRepeatsFor user 
        Logger.info $ "Sending echo:\n" <> text
                   <> "\nto: " .< user 
                   <> " " .< rep <> " times"
        replicateM_ rep (sendResponse @f url)
        
    SendHelpMessage user url -> do
        Logger.info $ "Sending help_message to " .< user
        sendResponse @f url

    SendRepeatMessage user url -> do
        Logger.info $ "Sending repeat_message to " .< user
        sendResponse @f url



handler = undefined

