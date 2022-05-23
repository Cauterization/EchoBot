module Bot.Bot where

import Bot.Error (BotError (BadCallbackError, ParsingError))
import Bot.FrontEnd
  ( Action (..),
    HasEnv (setRepeats),
    IsFrontEnd (getActions),
    getRepeatsFor,
  )
import Bot.IO (FrontEndIO (getUpdates, sendResponse), IsBot)
import Control.Monad (forever, replicateM_)
import Control.Monad.Catch (handle)
import Logger ((.<))
import Logger qualified
import Wait (MonadWait (wait))

runBot :: forall f m a. (IsBot f m) => m a
runBot = botHandler bot

bot :: forall f m a. (IsBot f m) => m a
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
  HideKeyboard user url -> do
    Logger.info $ "Hiding keyboard from " .< user
    sendResponse @f url
  SendEcho user text url -> do
    Logger.info $ "Sending echo:\n" <> text <> "\nto: " .< user
    sendResponse @f url
  SendRepeatEcho user text url -> do
    rep <- getRepeatsFor user
    Logger.info $
      "Sending echo:\n" <> text
        <> "\nto: " .< user
        <> " " .< rep
        <> " times"
    replicateM_ rep (sendResponse @f url)
  SendHelpMessage user url -> do
    Logger.info $ "Sending help_message to " .< user
    sendResponse @f url
  SendRepeatMessage user url -> do
    Logger.info $ "Sending repeat_message to " .< user
    sendResponse @f url

botHandler :: forall f m a. IsBot f m => m a -> m a
botHandler = handle $ \case
  ParsingError t -> do
    Logger.error t
    restartIn 30
  BadCallbackError t -> do
    Logger.error t
    restartIn 30

restartIn :: (IsBot f m) => Int -> m a
restartIn sec = do
  Logger.warning $ "Restarting in " .< sec <> " sec."
  wait sec
  runBot
