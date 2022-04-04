module Vkontakte.FrontEnd where

-- import Data.Aeson hiding (Key)
import Control.Monad.Catch

-- import Extended.Text (Text)
import Extended.Text (Text)
import Extended.Text qualified as T

-- import Bot.Error
import Bot.Types

-- import Data.Traversable
import Extended.HTTP qualified as HTTP 

import qualified Logger.Handle as Logger

import Bot.FrontEnd (IsFrontEnd, IsWebFrontEnd, Token(..), Action)
import Bot.FrontEnd qualified as Bot

import Vkontakte.Internal 

import Bot.Error
import GHC.Generics
import Data.Aeson
import Logger.Handle ((.<))
import qualified Data.ByteString.Lazy as BSL
import Control.Monad ((>=>))

data Vkontakte = Vkontakte deriving (Show, Generic, FromJSON)

instance IsFrontEnd Vkontakte where 

    type WebOnly    Vkontakte a = a

    type BotUser    Vkontakte = ID User

    type Update     Vkontakte = Update

    type FrontData  Vkontakte = FrontData

    newFrontData = newFrontData

    getActions = getActions
    
    prepareRequest = prepareRequest 

instance ( Monad m
         , MonadThrow m
         , HTTP.MonadHttp m
         , Logger.HasLogger m
         , Bot.HasEnv Vkontakte m
         ) => IsWebFrontEnd m Vkontakte where

    getUpdatesURL = getUpdatesURL

    type Response Vkontakte = GoodResponse 

    extractFrontData = fromTs . T.read . goodTs

    extractUpdates = updates

    type BadResponse Vkontakte = BadResponse

    handleBadResponse = handleBadResponse

    checkCallback = checkCallback

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
    , Bot.HasEnv Vkontakte m
    ) 
    => BadResponse -> m ()
handleBadResponse BadResponse{..} = case failed of
    1 -> Logger.warning "Update history is out of date. Updating TS... "
        >> maybe
            (Logger.error "Can't update TS - threre is no TS in this response!")
            (Bot.setFrontData . fromTs)
            badTs
    2 -> Logger.warning "Key is out of date. Getting new key..."
        >> Bot.getToken >>= newFrontData >>= Bot.setFrontData
    3 -> Logger.warning "FrontEnd data is lost, requesting new one..."
        >> Bot.getToken >>= newFrontData >>= Bot.setFrontData
    _ -> Logger.error "Unknown error code. IDK what to do with this."

checkCallback :: (Monad m, Logger.HasLogger m, MonadThrow m) => BSL.ByteString -> m ()
checkCallback = parse >=> \case
    GoodCallback _ -> pure ()
    BadCallback    -> throwM $ BadCallbackError ""

getActions :: (Monad m, Bot.HasEnv Vkontakte m, Logger.HasLogger m, MonadThrow m) 
    => Update -> m [Action Vkontakte]
getActions u = case u of

    RepeatUpdate userID 
        -> pure . Bot.SendRepeatMessage userID 
            <$> prepareRequest u
                           
    HelpUpdate userID   
        -> pure . Bot.SendHelpMessage userID 
            <$> prepareRequest u

    EchoUpdate text userID []
        -> pure . Bot.SendRepeatEcho userID text
            <$> prepareRequest u
                           
    EchoUpdate text userID as
        -> pure . Bot.SendEcho userID text 
            <$> prepareRequest u
                                              
    UpdateRepeats userID rep -> sequence 
        [ pure $ Bot.UpdateRepeats userID rep
        , Bot.HideKeyboard userID 
            <$> prepareRequest u
        ]
        
    Trash t -> [] <$
        Logger.debug ("That update doesn't look like something meaningful: " <> t)

prepareRequest :: (Monad m, Bot.HasEnv Vkontakte m) 
    => Update -> m URL
prepareRequest update = do
    let text = updateText update
        userID = updateUser update
        attachment = updateAttachment update
    token <-  ("&access_token=" <>) . unToken <$> Bot.getToken 
    message <- let str =  ("&message=" <>) in case text of
        ""        -> pure ""
        "/help"   -> str . HTTP.stringEncode <$> Bot.getHelpMessage
        "/repeat" -> str . HTTP.stringEncode <$> Bot.getRepeatMessage
        text      -> pure $ str text
    pure $ mconcat 
        [ "https://api.vk.com/method/messages.send"
        , "?user_id=" .< userID
        , message
        , token
        , "&v=5.81"
        , attachment
        ]

updateText :: Update -> Text
updateText = \case
    RepeatUpdate _ -> "/repeat"
    HelpUpdate _   -> "/help"
    EchoUpdate text _ _ -> text
    UpdateRepeats _ _ -> "repeats_updated"
    _ -> ""

updateUser :: Update -> ID User
updateUser = \case
    RepeatUpdate userID -> userID
    HelpUpdate userID   -> userID
    EchoUpdate _ userID [] -> userID
    EchoUpdate _ userID as -> userID
    UpdateRepeats userID _ -> userID
    _ -> 0

updateAttachment :: Update -> Text
updateAttachment = \case
    RepeatUpdate userID -> keyboard
    HelpUpdate userID   -> ""
    EchoUpdate _ userID [] -> ""
    EchoUpdate _ userID as -> prepareAttachment as
    UpdateRepeats userID _ -> hideKeyboard
    _ -> ""

prepareAttachment :: [Attachment] -> Text
prepareAttachment = \case
    [Attachment "sticker" sID _ _] -> "&sticker_id=" .< sID
    as ->  "&attachment=" <> attachmentsToReq as
  where    
    attachmentsToReq = T.intercalate "," . map attachmentToReq
    attachmentToReq Attachment{..} = mconcat
        [_type, T.show owner, "_", T.show _id, maybe "" ("_" <>) acessKey]
    
keyboard :: Text
keyboard = ("&keyboard=" <>) $ HTTP.percentEncode $ object 
    [ "buttons" .= [map keyboardButton [1..5 :: Integer]]
    , "inline"  .= False
    ]
  where  
    keyboardButton n = object 
        [ "action" .=  object 
            [ "type" .= ("callback" :: Text)
            , "label"   .= show n 
            , "payload" .= show n
            ]                   
        ] 

hideKeyboard :: Text
hideKeyboard = "&keyboard=" 
    <> HTTP.percentEncode @Text "{\"buttons\":[],\"inline\":false}"
