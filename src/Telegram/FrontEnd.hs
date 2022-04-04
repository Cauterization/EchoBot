module Telegram.FrontEnd where

import Control.Monad.Catch

import Extended.Text (Text)
import Extended.Text qualified as T

import Bot.Types

import Extended.HTTP qualified as HTTP 

import qualified Logger.Handle as Logger

import Bot.FrontEnd (IsFrontEnd, IsWebFrontEnd, Token(..), Action)
import Bot.FrontEnd qualified as Bot

import Telegram.Internal 

import Bot.Error
import GHC.Generics
import Data.Aeson
import Logger.Handle ((.<))
import qualified Data.ByteString.Lazy as BSL
import Data.String (fromString)

data Telegram = Telegram deriving (Show, Generic, FromJSON)

data BotUser = BotUser (ID User) (ID Chat) deriving (Show, Eq, Ord)

body :: Text
body = "https://api.vk.com/method/messages.send"

version :: Text
version = "&v=5.81"

instance IsFrontEnd Telegram where 

    type WebOnly    Telegram a = a

    type BotUser    Telegram = BotUser
 
    type Update     Telegram = Update
 
    type FrontData  Telegram = Offset

    newFrontData _ = pure mempty

    getActions = getActions

    prepareRequest = prepareRequest

instance ( Monad m
         , MonadThrow m
         , HTTP.MonadHttp m
         , Logger.HasLogger m
         , Bot.HasEnv Telegram m
         ) => IsWebFrontEnd m Telegram where

    getUpdatesURL = getUpdatesURL

    type Response Telegram = GoodResponse 

    extractFrontData GoodResponse{..} = mconcat $ map (Offset . getOffsetFromUpdate) result
      where
        getOffsetFromUpdate = \case
            EchoUpdate    (ID updateID) _ _ _ _ -> updateID
            RepeatUpdate  (ID updateID) _ _     -> updateID
            HelpUpdate    (ID updateID) _ _     -> updateID
            UpdateRepeats (ID updateID) _ _ _ _ -> updateID
            Trash         (ID updateID) _       -> updateID

    extractUpdates = result 

    type BadResponse Telegram = BadResponse

    handleBadResponse = handleBadResponse

    checkCallback = checkCallback

getActions :: (Monad m, Bot.HasEnv Telegram m, Logger.HasLogger m, MonadThrow m) 
    => Update -> m [Action Telegram]
getActions = \case
            
    u@(RepeatUpdate (ID updateID) userID chatID) -> do
        rMessage <- Bot.getRepeatMessage
        pure . Bot.SendRepeatMessage (BotUser userID chatID) <$> 
            prepareRequest u
               
    u@(HelpUpdate (ID updateID) userID chatID) -> do
        hMessage <- Bot.getHelpMessage
        pure . Bot.SendHelpMessage (BotUser userID chatID) <$> 
            prepareRequest u
                
    u@(EchoUpdate (ID updateID) userID chatID(ID messageID) (Just text)) -> 
        pure . Bot.SendRepeatEcho (BotUser userID chatID) text <$> 
            prepareRequest u
               
    u@(EchoUpdate (ID updateID) userID chatID (ID messageID) Nothing) -> 
        pure . Bot.SendEcho (BotUser userID chatID) "" <$> 
            prepareRequest u
              
    u@(UpdateRepeats (ID updateID) userID chatID (ID messageID) repeat) -> do
        req <- prepareRequest u
        pure [ Bot.UpdateRepeats (BotUser userID chatID) repeat
             , Bot.HideKeyboard (BotUser userID chatID) req
             ]

    Trash _ t -> [] <$ 
        Logger.debug ("That update doesn't look like something meaningful: " .< t)

prepareRequest :: forall m. (Monad m, Bot.HasEnv Telegram m) 
    => Update -> m URL
prepareRequest update = do
    token <- unToken <$> Bot.getToken  

    let chatID = case update of
            EchoUpdate    _ _ chatID _ _ -> chatID
            RepeatUpdate  _ _ chatID -> chatID
            HelpUpdate    _ _ chatID -> chatID
            UpdateRepeats _ _ chatID _ _ -> chatID 

        method = case update of
            UpdateRepeats{} -> "/editMessageReplyMarkup" 
            EchoUpdate{}    -> "/copyMessage" 
            _               -> "/sendMessage"

    rest <- case update of
        RepeatUpdate{} -> ("&text=" <>) . (<> keyboard) <$> Bot.getRepeatMessage
        HelpUpdate{} -> ("&text=" <>) <$> Bot.getHelpMessage 
        EchoUpdate _ _ (ID chatID) (ID messageID) _ -> pure $ 
            "&from_chat_id=" .< chatID <> "&message_id=" .< messageID
        UpdateRepeats _ _ _ messageID _ -> pure $ "&message_id=" .< messageID
        _ -> pure ""
        
    pure $ mconcat 
        [ "https://api.telegram.org/bot"
        , token
        , method
        , "?chat_id=" .< chatID
        , rest
        ]

getUpdatesURL :: Token Telegram -> Bot.FrontData Telegram -> PollingTime -> URL
getUpdatesURL (Token t) offset polling = mconcat 
    [ "https://api.telegram.org/bot"
    , t
    , "/getUpdates?offset="
    , T.show $ offset + 1
    , "&timeout="
    , T.show polling
    ]

keyboard :: Text
keyboard = ("&reply_markup=" <>) $ HTTP.percentEncode $ object 
    [ "inline_keyboard" .= [map keyboardButton [1..5 :: Integer]]
    , "onetime"         .= True
    ]
  where
    keyboardButton n = object
        [ "text"          .= show n
        , "callback_data" .= show n
        ]

checkCallback :: (Monad m, Logger.HasLogger m, MonadThrow m) => BSL.ByteString -> m ()
checkCallback cb = parse cb >>= \case
    GoodCallback     _          -> pure ()
    (BadCallback _ desc {-errCode desc-}) -> error $ show desc

handleBadResponse :: (Monad m, Logger.HasLogger m, MonadThrow m) => BadResponse -> m x
handleBadResponse {- b@(BadResponse errCode desc)-} = error . show 