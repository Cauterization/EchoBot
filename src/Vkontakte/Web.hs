module Vkontakte.Web where

-- import Data.Aeson hiding (Key)
import Control.Monad.Catch

import Data.Functor ( (<&>) )
-- import Extended.Text (Text)
import Extended.Text (Text)
import Extended.Text qualified as T

-- import Bot.Error
import Bot.Types

-- import Data.Traversable
import Extended.HTTP qualified as HTTP 

import qualified Logger.Handle as Logger

import FrontEnd.FrontEnd (IsFrontEnd, IsWebFrontEnd, Token(..), Action)
import FrontEnd.FrontEnd qualified as Front

import Vkontakte.FrontEnd 

import Bot.Error
import GHC.Generics
import Data.Aeson

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

checkCallback = undefined

getActions :: (Monad m, Front.HasEnv Vkontakte m) 
    => Update -> m [Action Vkontakte]
getActions = \case
        -- HelpUpdate    uID     -> pure $ Bot.Update $ SendHelp uID
        -- RepeatUpdate  uID     -> pure $ Bot.Update $ SendKeyboard uID
        -- UpdateRepeats uID rep ->
        --         pure $ Bot.UpdateRepeats (VKUser uID) rep $ HideKeyboard uID
        Update Message{..} -> pure . Front.SendEcho from_id <$> messageToEcho Message{..}
        Trash t            -> pure []


messageToEcho :: forall m. (Monad m, Front.HasEnv Vkontakte m) =>  Message -> m URL
messageToEcho Message{..} = do
    token <- ("&access_token=" <>) . unToken <$> Front.getToken 
    pure $ body <> user <> message <> token <> version <> attachment
  where
    user = "?user_id=" <> T.show from_id
    message = if T.null text then "" else "&message=" <> text
    attachment = case attachments of
            [Attachment "sticker" sID _ _] -> "&sticker_id=" <> T.show sID
            as ->  "&attachment=" <> attachmentsToReq as
    attachmentsToReq = T.intercalate "," . map attachmentToReq
    attachmentToReq Attachment{..} = mconcat
        [_type, T.show owner, "_", T.show _id, maybe "" ("_" <>) acessKey]

body :: Text
body = "https://api.vk.com/method/messages.send"

version :: Text
version = "&v=5.81"
