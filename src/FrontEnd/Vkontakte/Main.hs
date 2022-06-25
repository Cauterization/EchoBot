module FrontEnd.Vkontakte.Main where

import Bot.Error
  ( BotError (BadCallbackError),
    parse,
    parsingError,
  )
import Bot.FrontEnd qualified as Bot
import Bot.Types (ID, Token (unToken), URL)
import Bot.Web qualified as Bot
import Control.Lens ((.~))
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Reader ((>=>))
import Data.Aeson (KeyValue ((.=)), object)
import Data.ByteString.Lazy qualified as BSL
import Extended.HTTP qualified as HTTP
import Extended.Text (Text, readEither)
import Extended.Text qualified as T
import FrontEnd.Vkontakte.Config (VKConfig)
import FrontEnd.Vkontakte.Env
  ( VKEnv (..),
    envKey,
    envServer,
    envTs,
    getReponseWithFrontData,
    mkVkEnv,
    vkApiVersion,
  )
import FrontEnd.Vkontakte.Internal
  ( Attachment (..),
    BadResponse (..),
    Callback (BadCallback, GoodCallback),
    FrontDataResponse (key, server, ts),
    FrontUser,
    GoodResponse (..),
    Update (..),
    User,
  )
import Logger ((.<))
import Logger qualified
import Wait (MonadWait)

data Vkontakte

instance Bot.IsFrontEnd Vkontakte where
  type BotIOType Vkontakte = 'Bot.Web

  type BotConfig Vkontakte = VKConfig

  type BotFrontEnv Vkontakte = VKEnv

  mkFrontEnv = mkVkEnv

  type BotUser Vkontakte = FrontUser

  type Update Vkontakte = Update

  getActions = getActions

instance Bot.IsWebFrontEnd Vkontakte where
  getToken = envToken <$> Bot.getFrontEnv

  getPollingTime = envPollingTime <$> Bot.getFrontEnv

  getUpdatesURL = getUpdatesURL

  type Response Vkontakte = GoodResponse

  extractUpdates = updates

  updateFrontEnv = updateFrontEnv

  type BadResponse Vkontakte = BadResponse

  handleBadResponse = handleBadResponse

  checkCallback = checkCallback

getUpdatesURL :: (Monad m, Bot.HasEnv Vkontakte m) => m URL
getUpdatesURL = do
  VKEnv {..} <- Bot.getFrontEnv @Vkontakte
  pure $
    mconcat
      [ _envServer,
        "?act=a_check&key=",
        _envKey,
        "&ts=",
        T.show _envTs,
        "&wait=",
        T.show envPollingTime
      ]

getActions ::
  (Monad m, Bot.HasEnv Vkontakte m, Logger.HasLogger m, MonadThrow m) =>
  Update ->
  m [Bot.Action Vkontakte]
getActions u = case u of
  RepeatUpdate userID ->
    pure . Bot.SendRepeatMessage userID
      <$> prepareRequest u
  HelpUpdate userID ->
    pure . Bot.SendHelpMessage userID
      <$> prepareRequest u
  EchoUpdate text userID _ ->
    pure . Bot.SendEcho userID text
      <$> prepareRequest u
  UpdateRepeats userID rep ->
    sequence
      [ pure $ Bot.UpdateRepeats userID rep,
        Bot.HideKeyboard userID
          <$> prepareRequest u
      ]
  Trash t ->
    []
      <$ Logger.debug ("That update doesn't look like something meaningful: " <> t)

prepareRequest ::
  (Monad m, Bot.HasEnv Vkontakte m) =>
  Update ->
  m URL
prepareRequest update = do
  let text = updateText update
      userID = updateUser update
      attachment = updateAttachment update
  token <- ("&access_token=" <>) . unToken <$> Bot.getToken
  message <-
    let str = ("&message=" <>)
     in case text of
          "" -> pure ""
          "/help" -> str . HTTP.stringEncode <$> Bot.getHelpMessage
          "/repeat" -> str . HTTP.stringEncode <$> Bot.getRepeatMessage
          other -> pure $ str other
  pure $
    mconcat
      [ "https://api.vk.com/method/messages.send",
        "?user_id=" .< userID,
        message,
        token,
        "&v=",
        vkApiVersion,
        attachment
      ]

updateText :: Update -> Text
updateText = \case
  RepeatUpdate _ -> "/repeat"
  HelpUpdate _ -> "/help"
  EchoUpdate text _ _ -> text
  UpdateRepeats _ _ -> "repeats_updated"
  _ -> ""

updateUser :: Update -> ID User
updateUser = \case
  RepeatUpdate userID -> userID
  HelpUpdate userID -> userID
  EchoUpdate _ userID [] -> userID
  EchoUpdate _ userID _ -> userID
  UpdateRepeats userID _ -> userID
  _ -> 0

updateAttachment :: Update -> Text
updateAttachment = \case
  RepeatUpdate _ -> keyboard
  HelpUpdate _ -> ""
  EchoUpdate _ _ [] -> ""
  EchoUpdate _ _ as -> prepareAttachment as
  UpdateRepeats _ _ -> hideKeyboard
  _ -> ""

prepareAttachment :: [Attachment] -> Text
prepareAttachment = \case
  [Attachment "sticker" sID _ _] -> "&sticker_id=" .< sID
  as -> "&attachment=" <> attachmentsToReq as
  where
    attachmentsToReq = T.intercalate "," . map attachmentToReq
    attachmentToReq Attachment {..} =
      mconcat
        [_type, maybe "" T.show owner, "_", T.show _id, maybe "" ("_" <>) acessKey]

keyboard :: Text
keyboard =
  ("&keyboard=" <>) $
    HTTP.percentEncode $
      object
        [ "buttons" .= [map keyboardButton [1 .. 5 :: Integer]],
          "inline" .= False
        ]
  where
    keyboardButton n =
      object
        [ "action"
            .= object
              [ "type" .= ("callback" :: Text),
                "label" .= show n,
                "payload" .= show n
              ]
        ]

hideKeyboard :: Text
hideKeyboard =
  "&keyboard="
    <> HTTP.stringEncode "{\"buttons\":[],\"inline\":false}"

updateFrontEnv :: (Bot.HasEnv Vkontakte m, MonadThrow m) => GoodResponse -> m ()
updateFrontEnv GoodResponse {..}
  | Right ts' <- readEither goodTs = Bot.setFrontEnv (envTs .~ ts')
  | otherwise = parsingError "Can't read ts from vkontakte response."

checkCallback :: (Monad m, Logger.HasLogger m, MonadThrow m) => BSL.ByteString -> m ()
checkCallback =
  parse >=> \case
    GoodCallback _ -> pure ()
    BadCallback -> throwM $ BadCallbackError ""

handleBadResponse ::
  ( Monad m,
    MonadThrow m,
    HTTP.MonadHttp m,
    Logger.HasLogger m,
    MonadWait m,
    Bot.HasEnv Vkontakte m
  ) =>
  BadResponse ->
  m ()
handleBadResponse BadResponse {..} = case failed of
  1 -> do
    Logger.warning "Update history is out of date. Updating TS... "
    maybe
      (Logger.error "Can't update TS - threre is no TS in this response!")
      (Bot.setFrontEnv . (envTs .~))
      badTs
  2 -> do
    Logger.warning "Key is out of date. Getting new key..."
    t <- Bot.getToken
    groupID <- envGroupID <$> Bot.getFrontEnv
    pollingTime <- Bot.getPollingTime
    resp <- getReponseWithFrontData t groupID pollingTime
    Bot.setFrontEnv (envKey .~ key resp)
  3 -> do
    Logger.warning "FrontEnd data is lost, requesting new one..."
    t <- Bot.getToken
    groupID <- envGroupID <$> Bot.getFrontEnv
    pollingTime <- Bot.getPollingTime
    resp <- getReponseWithFrontData t groupID pollingTime
    Bot.setFrontEnv ((envKey .~ key resp) . (envTs .~ ts resp) . (envServer .~ server resp))
  _ -> Logger.error "Unknown error code. IDK what to do with this."
