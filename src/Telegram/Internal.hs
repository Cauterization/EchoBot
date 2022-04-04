{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Telegram.Internal where

import Data.Aeson 
import Deriving.Aeson

import Extended.Text (Text)
import Extended.Text qualified as T
import Data.Semigroup (Max(..))
import Bot.Types
import Data.Foldable

type ErrorCode = Int

type Description = Text

newtype Offset = Offset Int 
    deriving newtype (Show, Eq, Ord, Num, FromJSON)
    deriving (Semigroup, Monoid) via (Max Int)

newtype GoodResponse = GoodResponse {result :: [Update]} 
        deriving (Show, Generic, Eq)
        deriving anyclass FromJSON
        


        -- UpdateRepeats _ CallbackQ
        --     { _from = User userID
        --     , message = Message{ message_id = ID messageID , chat = Chat chatID }
        --     , _data = repText }
        --     -> do
        --     rep <- parse $ fromString $ T.unpack repText
        --     req <- prepareRequest 
        --             chatID  
        --             "/editMessageReplyMarkup" 
        --             ("&message_id=" .< messageID)
        --     pure [ Front.UpdateRepeats (BotUser userID chatID) rep
        --          , Front.HideKeyboard (BotUser userID chatID) req
        --          ]


data Update 
    = EchoUpdate    !(ID Update) !(ID User) !(ID Chat) !(ID Message) !(Maybe Text)
    | RepeatUpdate  !(ID Update) !(ID User) !(ID Chat)
    | HelpUpdate    !(ID Update) !(ID User) !(ID Chat)
    | UpdateRepeats !(ID Update) !(ID User) !(ID Chat) !(ID Message) !Repeat
    | Trash         !(ID Update) !Object
    deriving (Show, Generic, Eq)

instance FromJSON Update where
    parseJSON = withObject "Update" $ \v -> do
        updateID <- v .: "update_id"
        asum 
            [ do
                Message{..} <- v .: "message"  
                let userID = u_id from
                    chatID = c_id chat
                pure $ case text of
                    Just "/help" 
                        -> HelpUpdate updateID userID chatID
                    Just "/repeat"
                        -> RepeatUpdate updateID userID chatID
                    t   -> EchoUpdate updateID userID chatID message_id t       
            , do
                CallbackQ User{..} Message{..} _data <- v .: "callback_query"
                let chatID = c_id chat
                    messageID = message_id
                case T.readEither _data of
                    Right rep 
                        -> pure $ UpdateRepeats updateID u_id chatID messageID rep
                    Left err -> fail $ T.unpack err
            , pure $ Trash updateID v
            ]

data CallbackQ = CallbackQ
    { _from   :: !User
    , message :: !Message
    , _data   :: !Text
    } deriving stock (Show, Generic, Eq)
      deriving (FromJSON) via 
        (CustomJSON '[FieldLabelModifier (StripPrefix "_")] CallbackQ)

-- pattern UpdateRepeats :: ID User -> ID Message -> ID Chat -> Text -> Update
-- pattern UpdateRepeats uID mID cID repText
--     <- CallbackUpdate _ CallbackQ
--         { _from = User{id = uID}
--         , message = Message{message_id = mID, chat = Chat{id = cID}}
--         , _data = repText
--         }

data Message = Message 
    { message_id :: !(ID Message)
    , from       :: !User
    , chat       :: !Chat
    , text       :: !(Maybe Text)
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON)

newtype User = User { u_id :: ID User } 
    deriving stock (Show, Generic, Eq)
    deriving FromJSON via CustomJSON '[FieldLabelModifier '[StripPrefix "u_"]] User

newtype Chat = Chat { c_id :: ID Chat } 
    deriving stock (Show, Generic, Eq)
    deriving FromJSON via CustomJSON '[FieldLabelModifier '[StripPrefix "c_"]] Chat

data BadResponse = BadResponse {errorCode :: !ErrorCode, description :: !Text} 
    deriving (Show, Generic, Eq)
    deriving FromJSON via CustomJSON '[FieldLabelModifier CamelToSnake] BadResponse

data Callback
    = BadCallback ErrorCode Description  
    | GoodCallback ()
    deriving (Eq, Show, Generic)
    deriving FromJSON via CustomJSON '[SumUntaggedValue] Callback
