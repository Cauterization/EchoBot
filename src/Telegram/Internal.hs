{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Telegram.Internal where

import Data.Aeson 
import Deriving.Aeson
-- import Data.Aeson.Key (toText)

import Extended.Text (Text)
import Extended.Text qualified as T
import Data.Semigroup (Max(..))
-- import Bot.Error
import Bot.Types
import GHC.Generics (Generic)
import Control.Applicative

type ErrorCode = Int

type Description = Text

newtype Offset = Offset Int 
    deriving newtype (Show, Eq, Ord, Num, FromJSON)
    deriving Monoid via (Max Int)

instance Semigroup Offset where
    (<>) = ((+1) .) . max 

newtype GoodResponse = GoodResponse {result :: [Update]} 
        deriving (Show, Generic, Eq)
        deriving anyclass FromJSON
        
data Update 
    = Update        !(ID Update) !Message
    | CallbackUpdate !(ID Update) !CallbackQ
    | Trash         !(ID Update) !Object
    deriving (Show, Generic, Eq)
instance FromJSON Update where
    parseJSON = withObject "Update" $ \v -> do
            Update        <$> v .: "update_id" <*> v .: "message"        
        <|> CallbackUpdate <$> v .: "update_id" <*> v .: "callback_query" 
        <|> Trash         <$> v .: "update_id" <*> pure v

data CallbackQ = CallbackQ
    { _from    :: !User
    , message :: !Message
    , _data   :: !Text
    } deriving stock (Show, Generic, Eq)
      deriving (FromJSON) via (CustomJSON '[FieldLabelModifier (StripPrefix "_")] CallbackQ)

pattern UpdateRepeats :: ID User -> ID Message -> ID Chat -> Text -> Update
pattern UpdateRepeats uID mID cID repText
    <- CallbackUpdate _ CallbackQ{ _from = User{id = uID}
                               , message = Message{message_id = mID, chat = Chat{id = cID}}
                               , _data = repText
                               }

data Message = Message 
    { message_id :: !(ID Message)
    , from       :: !User
    , chat       :: !Chat
    , text       :: !(Maybe Text)
    } deriving stock (Show, Generic, Eq)
      deriving anyclass (FromJSON)

pattern EchoUpdate :: ID Message -> ID User -> ID Chat -> Update
pattern EchoUpdate mID uID cID
    <- Update _ Message{message_id = mID,from = User{id = uID}, chat = Chat{id = cID}}

pattern RepeatUpdate, HelpUpdate :: ID Chat -> Update
pattern RepeatUpdate cID 
    <- Update _ Message{from = User{id = uID}, chat = Chat{id = cID}, text = Just "/repeat"}
pattern HelpUpdate cID 
    <- Update _ Message{from = User{id = uID}, chat = Chat{id = cID}, text = Just "/help"}

newtype User = User { id :: ID User } 
    deriving stock (Show, Generic, Eq)
    deriving anyclass (FromJSON)

newtype Chat = Chat { id :: ID Chat } 
    deriving stock (Show, Generic, Eq)
    deriving anyclass (FromJSON)

data BadResponse = BadResponse {errorCode :: !ErrorCode, description :: !Text} 
    deriving (Show, Generic, Eq)
    deriving FromJSON via 
        CustomJSON '[FieldLabelModifier CamelToSnake] BadResponse

data Callback
    = BadCallback ErrorCode Description  
    | GoodCallback ()
    deriving (Eq, Show, Generic)
    deriving FromJSON via
        CustomJSON '[SumUntaggedValue] Callback
