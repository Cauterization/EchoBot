module Vkontakte.Web where

-- import Data.Aeson hiding (Key)
import Control.Monad.Catch

import Data.Functor ( (<&>), ($>) )
-- import Extended.Text (Text)
import Extended.Text (Text)
import Extended.Text qualified as T

-- import Bot.Error
import Bot.Types

-- import Data.Traversable
import Extended.HTTP qualified as HTTP 

import qualified Logger.Handle as Logger

import Bot.FrontEnd (IsFrontEnd, IsWebFrontEnd, Token(..), Action)
import Bot.FrontEnd qualified as Front

import Vkontakte.FrontEnd 

import Bot.Error
import GHC.Generics
import Data.Aeson
import Logger.Handle ((.<))
import qualified Data.ByteString.Lazy as BSL
import Control.Monad ((>=>))

data Vkontakte = Vkontakte deriving (Show, Generic, FromJSON)

{-
>>> eitherDecode @(Front.FrontName Vkontakte) "Vkontakte"
Left "Error in $: Failed reading: not a valid json value at 'Vkontakte'"
-}

instance IsFrontEnd Vkontakte where 

    type WebOnly Vkontakte a = a

    type User      Vkontakte = ID User

    type Update    Vkontakte = Update

    type FrontData Vkontakte = FrontData

    newFrontData = newFrontData

    -- type SendEcho 'Vkontakte = User
    -- type SendHelp 'Vkontakte = User
    -- type SendKeyboard  f :: Type
    -- type HideKeyboard  f :: Type

    -- getActions :: GoodResponse -> [Action 'Vkontakte]
    getActions = getActions

instance ( Monad m
         , MonadThrow m
         , HTTP.MonadHttp m
         , Logger.HasLogger m
         , Front.HasEnv Vkontakte m
         ) => IsWebFrontEnd Vkontakte m where

    getUpdatesURL = getUpdatesURL

    type Response Vkontakte = GoodResponse 

    extractFrontData = fromTs . T.read . goodTs

    -- type SendKeyboard Vkontakte = ID User

    -- type HideKeyboard Vkontakte = ID User

    extractUpdates = updates

    type BadResponse Vkontakte = BadResponse

    handleBadResponse = handleBadResponse

    checkCallback = checkCallback

    -- newFrontData = newFrontData

newFrontData :: (Monad m, HTTP.MonadHttp m, MonadThrow m) 
    => Token f -> m FrontData
newFrontData (Token t) = do
    let req = "https://api.vk.com/method/groups.getLongPollServer"
           <> "?group_id=204518764"
           <> "&access_token=" <> t
           <> "&v=5.81"
    HTTP.tryRequest req >>= parse

getUpdatesURL :: Token Vkontakte -> FrontData -> PollingTime -> URL
getUpdatesURL _ FrontData{..} polling = mconcat 
    [ server 
    , "?act=a_check&key=", key
    , "&ts=", T.show ts
    , "&wait=", T.show polling
    ]

handleBadResponse :: 
    ( Monad m
    , MonadThrow m
    , HTTP.MonadHttp m
    , Logger.HasLogger m
    , Front.HasEnv Vkontakte m
    ) 
    => BadResponse -> m ()
handleBadResponse BadResponse{..} = case failed of
    1 -> Logger.warning "Update history is out of date. Updating TS... "
        >> maybe
            (Logger.error "Can't update TS - threre is no TS in this response!")
            (Front.setFrontData . fromTs)
            badTs
    2 -> Logger.warning "Key is out of date. Getting new key..."
        >> Front.getToken >>= newFrontData >>= Front.setFrontData
    3 -> Logger.warning "FrontEnd data is lost, requesting new one..."
        >> Front.getToken >>= newFrontData >>= Front.setFrontData
    _ -> Logger.error "Unknown error code. IDK what to do with this."

checkCallback :: (Monad m, Logger.HasLogger m, MonadThrow m) => BSL.ByteString -> m ()
checkCallback = parse >=> \case
    GoodCallback _ -> pure ()
    BadCallback    -> throwM $ BadCallbackError ""

getActions :: (Monad m, Front.HasEnv Vkontakte m, Logger.HasLogger m) 
    => Update -> m [Action Vkontakte]
getActions = \case
        -- HelpUpdate    uID     -> pure $ Bot.Update $ SendHelp uID
        -- RepeatUpdate  uID     -> pure $ Bot.Update $ SendKeyboard uID
        -- UpdateRepeats uID rep ->
        --         pure $ Bot.UpdateRepeats (VKUser uID) rep $ HideKeyboard uID
        Update Message{..} 
            -> pure . Front.SendEcho from_id <$> mkEchoRequest Message{..}
        UpdateRepeats userID rep 
            -> sequence [ pure $ Front.UpdateRepeats userID rep
                        , Front.HideKeyboard <$> mkHideKeyboardRequest userID
                        ]
        Trash t 
            -> [] <$ Logger.debug ("That update doesn't look like something meaningful " <> t)


mkEchoRequest :: forall m. (Monad m, Front.HasEnv Vkontakte m) =>  Message -> m URL
mkEchoRequest Message{..} = do
    token <- prepareToken
    pure $ mconcat [ body
                   , prepareUser from_id
                   , if T.null text then "" else prepareMessage text
                   , token
                   , version
                   , attachment
                   ]
  where
    attachment = case attachments of
            [Attachment "sticker" sID _ _] -> "&sticker_id=" <> T.show sID
            as ->  "&attachment=" <> attachmentsToReq as
    attachmentsToReq = T.intercalate "," . map attachmentToReq
    attachmentToReq Attachment{..} = mconcat
        [_type, T.show owner, "_", T.show _id, maybe "" ("_" <>) acessKey]

mkHideKeyboardRequest :: forall m. (Monad m, Front.HasEnv Vkontakte m) => ID User -> m URL
mkHideKeyboardRequest userID = do
    token <- prepareToken
    pure $ mconcat [ body
                   , prepareUser userID
                   , prepareMessage "repeats_updated"
                   , token
                   , version
                   , "&keyboard="
                   , HTTP.percentEncode ("{\"buttons\":[],\"inline\":false}" :: Text)
                   ]

prepareMessage :: Text -> Text
prepareMessage = ("&message=" <>)

prepareUser :: ID User -> Text
prepareUser userID = "?user_id=" <> T.show userID

prepareToken :: (Front.HasEnv Vkontakte m, Functor m) => m Text
prepareToken = ("&access_token=" <>) . unToken <$> Front.getToken 

body :: Text
body = "https://api.vk.com/method/messages.send"

version :: Text
version = "&v=5.81"
