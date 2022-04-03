module Vkontakte.FrontEnd where

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

        RepeatUpdate userID 
            -> pure . Front.SendEcho <$> prepareRequest "/repeat" userID keyboard

        HelpUpdate userID
            -> pure . Front.SendEcho <$> prepareRequest "/help" userID ""

        AttachmentUpdate userID text as
            -> pure . Front.SendEcho <$> prepareRequest text userID (prepareAttachment as)

        Update Message{..} 
            -> pure . Front.SendRepeatEcho from_id 
                <$> prepareRequest text from_id ""

        UpdateRepeats userID rep 
            -> sequence 
                [ pure $ Front.UpdateRepeats userID rep
                , Front.HideKeyboard 
                    <$> prepareRequest "repeats_updated" userID hideKeyboard
                ]
        Trash t 
            -> [] <$ Logger.debug ("That update doesn't look like something meaningful: " <> t)


prepareRequest :: forall m. (Monad m, Front.HasEnv Vkontakte m) 
    =>  Text -> ID User -> Text -> m URL
prepareRequest m userID attachment = do
    token <-  ("&access_token=" <>) . unToken <$> Front.getToken 
    message <- let str =  ("&message=" <>) in case m of
        ""        -> pure ""
        "/help"   -> str . HTTP.stringEncode <$> Front.getHelpMessage
        "/repeat" -> str . HTTP.stringEncode <$> Front.getRepeatMessage
        text      -> pure $ str text
    pure $ mconcat 
        [ "https://api.vk.com/method/messages.send"
        , "?user_id=" <> T.show  userID
        , message
        , token
        , "&v=5.81"
        , attachment
        ]

prepareAttachment :: [Attachment] -> Text
prepareAttachment = \case
    [Attachment "sticker" sID _ _] -> "&sticker_id=" <> T.show sID
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
