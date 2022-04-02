{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Vkontakte.FrontEnd where

import Data.Aeson hiding (Key)

import Extended.Text (Text)
import Extended.Text qualified as T

import Bot.Error
import Bot.Types
import GHC.Generics (Generic)
import qualified Extended.HTTP as HTTP
import Control.Monad.Catch
import Data.Functor ((<&>))
import Control.Applicative
import Deriving.Aeson
    ( Generic,
      CamelToSnake,
      CustomJSON(CustomJSON),
      FieldLabelModifier,
      StripPrefix
      )
import qualified Logger.Handle as Logger
import Data.Function (on)

data User
type Key       = T.Text
type Server    = T.Text
type Ts        = Int
type ErrorCode = Int

type FrontUser = ID User

data FrontData = FrontData
    { key    :: !Key
    , server :: !Server
    , ts     :: !Ts
    } deriving (Show, Generic, Eq, ToJSON)

instance Semigroup FrontData where
    a <> b = FrontData k s t
      where
        k = key $ if T.null (key a) then b else a
        s = server $ if T.null (server a) then b else a
        t = (max `on` ts) a b

instance Monoid FrontData where
    mempty = fromTs 0

fromTs :: Ts -> FrontData 
fromTs = FrontData "" ""

instance FromJSON FrontData where
    parseJSON = withObject "FrontData VK" $ \v -> do
        r        <- v .: "response"
        key      <- r .: "key"
        server   <- r .: "server"
        Right ts <- r .: "ts" <&> T.readEither
        pure $ FrontData{..}

data GoodResponse = GoodResponse { goodTs :: !Text, updates :: [Update]}
    deriving (Show, Generic)
    deriving (FromJSON) via 
        CustomJSON '[FieldLabelModifier '[StripPrefix "good", CamelToSnake]] GoodResponse

{-
>>> eitherDecode @GoodResponse "{\"ts\":\"1265\",\"updates\":[{\"type\":\"message_typing_state\",\"event_id\":\"d3fbf3df74251445730465484c72e666e43c6591\",\"v\":\"5.81\",\"object\":{\"state\":\"typing\",\"from_id\":88659146,\"to_id\":-204518764},\"group_id\":204518764}]}\r\n"
Right (GoodResponse {goodTs = "1265", updates = [Trash "message_typing_state"]})
-}

data Update = Update        !Message
            | UpdateRepeats !(ID User)  !Repeat
            | Trash         !T.Text
            deriving (Show, Generic)

instance FromJSON Update where
    parseJSON  = withObject "Update" $ \v -> do
        t <- v .: "type"
        case t of
            "message_new"   -> do 
                o <- v .: "object"
                pure $ Update o
            "message_event" -> do
                o       <- v .: "object"
                userID  <- o .: "user_id"
                payload <- o .: "payload"
                pure $ UpdateRepeats userID payload
            _  -> pure $ Trash t


data Message = Message
    { from_id      :: !(ID User)
    , text         :: !Text
    , fwd_messages :: [Object]
    , attachments  :: [Attachment]
    } deriving (Show, Generic, FromJSON)

data Attachment = Attachment 
    { _type :: !T.Text 
    , _id   :: !(ID Attachment)
    , owner :: !(ID User) 
    , acessKey  :: !(Maybe T.Text) 
    } deriving (Show, Generic)

instance FromJSON Attachment where
    parseJSON = withObject "VK_Attachment" $ \v -> do
        t        <- v .: "type"
        let _type = T.show t
        inner    <- v .: t
        _id      <- inner .:  "sticker_id" <|> inner .: "id"
        owner    <- inner .:  "owner_id"   <|> inner .: "from_id"
        acessKey <- inner .:? "access_key"
        pure $ Attachment{..}

data BadResponse = BadResponse {failed :: !ErrorCode, badTs :: !(Maybe Ts)} 
    deriving (Show, Generic, Eq)
    deriving (FromJSON, ToJSON) via 
        CustomJSON '[FieldLabelModifier '[StripPrefix "bad", CamelToSnake]] BadResponse


pattern RepeatUpdate, HelpUpdate :: ID User -> Update
pattern RepeatUpdate uID 
    <- Update Message{from_id = uID, text = "/repeat"}
pattern HelpUpdate uID 
    <- Update Message{from_id = uID, text = "/help"}






-- data Command = SendEchoCommand      Message
--              | SendHelpCommand      (ID User)
--              | SendKeyboardCommand  (ID User)
--              | UpdateRepeatsCommand (ID User) Repeat
--              | HideKeyboardCommand  (ID User)
--              | DoNothingCommand
    -- Trash t               -> [DoNothing]
