module FrontEnd.Telegram.Main where

import Bot.Error (BotError (BadCallbackError), parse)
import Bot.FrontEnd qualified as Bot
import Bot.Types (ID (ID), Token (unToken), URL)
import Bot.Web qualified as Bot
import Control.Lens ((.~))
import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson (KeyValue ((.=)), object)
import Data.ByteString.Lazy qualified as BSL
import Extended.HTTP qualified as HTTP
import Extended.Text (Text)
import Extended.Text qualified as T
import FrontEnd.Telegram.Config (TGConfig)
import FrontEnd.Telegram.Env (TGEnv (..), envOffset, mkTGEnv)
import FrontEnd.Telegram.Internal
  ( BadResponse (BadResponse),
    Callback (BadCallback, GoodCallback),
    Chat,
    GoodResponse (..),
    Update (..),
    User,
  )
import Logger ((.<))
import Logger qualified

data Telegram

instance Bot.IsFrontEnd Telegram where
  type BotIOType Telegram = 'Bot.Web

  type BotConfig Telegram = TGConfig

  type BotFrontEnv Telegram = TGEnv

  mkFrontEnv = mkTGEnv

  type BotUser Telegram = BotUser

  type Update Telegram = Update

  getActions = getActions

instance Bot.IsWebFrontEnd Telegram where
  getToken = envToken <$> Bot.getFrontEnv

  getUpdatesURL = getUpdatesURL

  type Response Telegram = GoodResponse

  extractUpdates = result

  updateFrontEnv = updateFrontEnv

  type BadResponse Telegram = BadResponse

  handleBadResponse = handleBadResponse

  checkCallback = checkCallback

data BotUser = BotUser (ID User) (ID Chat) deriving (Show, Eq, Ord)

getActions ::
  (Monad m, Bot.HasEnv Telegram m, Logger.HasLogger m, MonadThrow m) =>
  Update ->
  m [Bot.Action Telegram]
getActions = \case
  u@(RepeatUpdate _ userID chatID) -> do
    pure . Bot.SendRepeatMessage (BotUser userID chatID)
      <$> prepareRequest u
  u@(HelpUpdate _ userID chatID) -> do
    pure . Bot.SendHelpMessage (BotUser userID chatID)
      <$> prepareRequest u
  u@(EchoUpdate _ userID chatID _ (Just text)) ->
    pure . Bot.SendRepeatEcho (BotUser userID chatID) text
      <$> prepareRequest u
  u@(EchoUpdate _ userID chatID _ Nothing) ->
    pure . Bot.SendEcho (BotUser userID chatID) ""
      <$> prepareRequest u
  u@(UpdateRepeats _ userID chatID _ newRep) -> do
    req <- prepareRequest u
    pure
      [ Bot.UpdateRepeats (BotUser userID chatID) newRep,
        Bot.HideKeyboard (BotUser userID chatID) req
      ]
  Trash _ t ->
    []
      <$ Logger.debug ("That update doesn't look like something meaningful: " .< t)

prepareRequest ::
  forall m.
  (Monad m, Bot.HasEnv Telegram m) =>
  Update ->
  m URL
prepareRequest update = do
  token <- unToken . envToken <$> Bot.getFrontEnv

  let chatID = case update of
        EchoUpdate _ _ c _ _ -> c
        RepeatUpdate _ _ c -> c
        HelpUpdate _ _ c -> c
        UpdateRepeats _ _ c _ _ -> c
        Trash {} -> 0

      method = case update of
        UpdateRepeats {} -> "/editMessageReplyMarkup"
        EchoUpdate {} -> "/copyMessage"
        _ -> "/sendMessage"

  rest <- case update of
    RepeatUpdate {} -> ("&text=" <>) . (<> keyboard) <$> Bot.getRepeatMessage
    HelpUpdate {} -> ("&text=" <>) <$> Bot.getHelpMessage
    EchoUpdate _ _ (ID chatID') (ID messageID) _ ->
      pure $
        "&from_chat_id=" .< chatID' <> "&message_id=" .< messageID
    UpdateRepeats _ _ _ messageID _ -> pure $ "&message_id=" .< messageID
    _ -> pure ""

  pure $
    mconcat
      [ "https://api.telegram.org/bot",
        token,
        method,
        "?chat_id=" .< chatID,
        rest
      ]

getUpdatesURL :: (Monad m, Bot.HasEnv Telegram m) => m URL
getUpdatesURL = do
  TGEnv {..} <- Bot.getFrontEnv @Telegram
  pure $
    mconcat
      [ "https://api.telegram.org/bot",
        unToken envToken,
        "/getUpdates?offset=",
        T.show $ _envOffset + 1,
        "&timeout=",
        T.show envPollingTime
      ]

keyboard :: Text
keyboard =
  ("&reply_markup=" <>) $
    HTTP.percentEncode $
      object
        [ "inline_keyboard" .= [map keyboardButton [1 .. 5 :: Integer]],
          "onetime" .= True
        ]
  where
    keyboardButton n =
      object
        [ "text" .= show n,
          "callback_data" .= show n
        ]

checkCallback :: (Monad m, Logger.HasLogger m, MonadThrow m) => BSL.ByteString -> m ()
checkCallback cb =
  parse cb >>= \case
    GoodCallback _ -> pure ()
    (BadCallback _ desc) -> throwM $ BadCallbackError desc

updateFrontEnv :: (Bot.HasEnv Telegram m, MonadThrow m) => GoodResponse -> m ()
updateFrontEnv GoodResponse {..} = case result of
  [] -> pure ()
  us -> Bot.setFrontEnv $ (envOffset .~) $ getOffset $ last us
  where
    getOffset = \case
      EchoUpdate (ID updateID) _ _ _ _ -> updateID
      RepeatUpdate (ID updateID) _ _ -> updateID
      HelpUpdate (ID updateID) _ _ -> updateID
      UpdateRepeats (ID updateID) _ _ _ _ -> updateID
      Trash (ID updateID) _ -> updateID

handleBadResponse :: (Monad m, Logger.HasLogger m, MonadThrow m) => BadResponse -> m ()
handleBadResponse (BadResponse errCode desc) = case errCode of
  303 -> Logger.error $ "SEE_OTHER: " <> desc
  400 -> Logger.error $ "BAD_REQUEST: " <> desc
  401 -> Logger.error $ "UNAUTHORIZED: " <> desc
  403 -> Logger.error $ "FORBIDDEN: " <> desc
  404 -> Logger.error $ "NOT_FOUND: " <> desc
  406 -> Logger.error $ "NOT_ACCEPTABLE: " <> desc
  420 -> Logger.error $ "FLOOD: " <> desc
  500 -> Logger.error $ "INTERNAL TG ERROR: " <> desc
  _ -> do
    Logger.error "UNKNOWN TG ERROR."
    Logger.error $ "Error code: " .< T.show errCode
    Logger.error $ " Description " <> desc
