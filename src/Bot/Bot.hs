module Bot.Bot where

import Bot.Error (BotError (BadCallbackError, ParsingError))
import Bot.FrontEnd
  ( Action (..),
    HasEnv (setRepeats),
    IsFrontEnd (getActions),
    getRepeatsFor,
  )
import Bot.IO (FrontEndIO (getUpdates, sendResponse), MonadBot)
import Control.Monad (forever, replicateM_)
import Control.Monad.Catch (fromException, handleAll)
import Extended.Text qualified as T
import Logger ((.<))
import Logger qualified
import Wait (MonadWait (wait))

runBot :: forall f m a. (MonadBot f m) => m a
runBot = botHandler bot

bot :: forall f m a. (MonadBot f m) => m a
bot = forever $ recieveActions @f >>= mapM_ (executeAction @f)

recieveActions :: forall f m. MonadBot f m => m [Action f]
recieveActions = do
  Logger.info "Getting updates.."
  updates <- getUpdates @f
  Logger.info $ "Recieved " .< length updates <> " new updates."
  concat <$> mapM getActions updates

executeAction :: forall f m. MonadBot f m => Action f -> m ()
executeAction = \case
  UpdateRepeats user rep -> do
    Logger.info $ "New number of repeations for user " .< user <> " is " .< rep
    setRepeats user rep
  HideKeyboard user toSend -> do
    Logger.info $ "Hiding keyboard from " .< user
    sendResponse @f toSend
  SendEcho user text toSend -> do
    rep <- getRepeatsFor user
    Logger.info $
      "Sending echo:\n" <> text
        <> "\nto: " .< user
        <> " " .< rep
        <> " times"
    replicateM_ rep (sendResponse @f toSend)
  SendHelpMessage user toSend -> do
    Logger.info $ "Sending help_message to " .< user
    sendResponse @f toSend
  SendRepeatMessage user toSend -> do
    Logger.info $ "Sending repeat_message to " .< user
    sendResponse @f toSend

botHandler :: forall f m a. MonadBot f m => m a -> m a
botHandler = handleAll $ \err -> case fromException err of
  Just (ParsingError t) -> do
    Logger.error t
    restartIn 30
  Just (BadCallbackError t) -> do
    Logger.error t
    restartIn 30
  _ -> do
    Logger.error $ T.show err
    restartIn 30

restartIn :: (MonadBot f m) => Int -> m a
restartIn sec = do
  Logger.warning $ "Restarting in " .< sec <> " sec."
  wait sec
  runBot
