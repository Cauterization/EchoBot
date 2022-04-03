{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

module Vkontakte.Internal where

import Data.Aeson hiding (Key)
import Data.Aeson.Key (toText)

import Extended.Text (Text)
import Extended.Text qualified as T

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

data Update = Update        !Message
            | UpdateRepeats !(ID User)  !Repeat
            | Trash         !T.Text
            deriving (Show, Generic)

pattern RepeatUpdate, HelpUpdate :: ID User -> Update
pattern RepeatUpdate userID 
    <- Update Message{from_id = userID, text = "/repeat"}
pattern HelpUpdate userID 
    <- Update Message{from_id = userID, text = "/help"}

pattern AttachmentUpdate ::  ID User -> Text -> [Attachment] -> Update
pattern AttachmentUpdate userID text as
    <- Update Message{from_id = userID, text = text, attachments = as@(_:_)}

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
        let _type = toText t
        inner    <- v .: t
        _id      <- inner .:  "sticker_id" <|> inner .: "id"
        owner    <- inner .:  "owner_id"   <|> inner .: "from_id"
        acessKey <- inner .:? "access_key"
        pure $ Attachment{..}

data BadResponse = BadResponse {failed :: !ErrorCode, badTs :: !(Maybe Ts)} 
    deriving (Show, Generic, Eq)
    deriving (FromJSON, ToJSON) via 
        CustomJSON '[FieldLabelModifier '[StripPrefix "bad", CamelToSnake]] BadResponse

data Callback 
    = GoodCallback Int
    | BadCallback 
    deriving (Eq, Show, Generic)

instance FromJSON Callback where
    parseJSON = withObject "Callback" $ \v -> do
        GoodCallback <$> v .: "response"
        <|> pure BadCallback
