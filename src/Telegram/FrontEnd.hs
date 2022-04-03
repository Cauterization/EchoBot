module Telegram.FrontEnd where

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

import Telegram.Internal 

import Bot.Error
import GHC.Generics
import Data.Aeson
import Logger.Handle ((.<))
import qualified Data.ByteString.Lazy as BSL
import Control.Monad ((>=>))
import Data.Traversable
import Data.String (fromString)

data Telegram = Telegram deriving (Show, Generic, FromJSON)

data BotUser = BotUser (ID User) (ID Chat) deriving (Eq, Ord)

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

instance ( Monad m
         , MonadThrow m
         , HTTP.MonadHttp m
         , Logger.HasLogger m
         , Front.HasEnv Telegram m
         ) => IsWebFrontEnd Telegram m where

    getUpdatesURL = getUpdatesURL

    type Response Telegram = GoodResponse 

    extractFrontData GoodResponse{..} = mconcat $ map (Offset . getOffsetFromUpdate) result
      where
        getOffsetFromUpdate = \case
            Update         (ID i) _ -> i
            CallbackUpdate (ID i) _ -> i
            Trash          (ID i) _ -> i

    extractUpdates = result 

    type BadResponse Telegram = BadResponse

    handleBadResponse = handleBadResponse

    checkCallback = checkCallback

getActions :: (Monad m, Front.HasEnv Telegram m, Logger.HasLogger m, MonadThrow m) 
    => Update -> m [Action Telegram]
getActions = \case

        RepeatUpdate userID chatID 
            -> fmap (pure . Front.SendEcho) . prepareRequest userID chatID "/sendMessage" 
                    . ("&text=" <>) . (<> keyboard) =<< Front.getRepeatMessage

        HelpUpdate userID chatID
            -> pure . Front.SendEcho 
                <$> (prepareRequest  userID chatID "/sendMessage" . ("&text=" <>) 
                =<< Front.getHelpMessage)

        EchoUpdate messageID userID chatID 
            -> fmap (pure . Front.SendRepeatEcho (BotUser userID chatID))
                $ prepareRequest userID chatID "/copyMessage"  
                    $ "&from_chat_id=" <> T.show chatID <> "&message_id=" <> T.show messageID

        UpdateRepeats userID messageID chatID repText
            -> do
            rep <- parse $ fromString $ T.unpack repText
            sequence 
                [ pure $ Front.UpdateRepeats (BotUser userID chatID) rep
                , fmap Front.HideKeyboard 
                    . prepareRequest userID chatID  "/editMessageReplyMarkup" 
                        $ "&message_id=" <> T.show messageID
                ]

        Trash _ t 
            -> [] <$ Logger.debug ("That update doesn't look like something meaningful: " .< t)

        _ -> error "unknown update"

prepareRequest :: forall m. (Monad m, Front.HasEnv Telegram m) 
    => ID User -> ID Chat -> Text -> Text -> m URL
prepareRequest userID chatID method rest = do
    token <- unToken <$> Front.getToken    
    pure $ mconcat 
        [ "https://api.telegram.org/bot"
        , token
        , method
        , "?chat_id=" <> T.show chatID
        , rest
        ]

getUpdatesURL :: Token Telegram -> Front.FrontData Telegram -> PollingTime -> URL
getUpdatesURL (Token t) offset polling = mconcat 
    [ "https://api.telegram.org/bot"
    , t
    , "/getUpdates?offset="
    , T.show offset 
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
    (BadCallback errCode desc) -> undefined

handleBadResponse :: (Monad m, Logger.HasLogger m, MonadThrow m) => BadResponse -> m x
handleBadResponse b@(BadResponse errCode desc) = error $ show b