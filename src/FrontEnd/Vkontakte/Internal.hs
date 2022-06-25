{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}

module FrontEnd.Vkontakte.Internal where

import Bot.Types (ID, Repeat)
import Control.Applicative (Alternative ((<|>)))
import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    ToJSON,
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Key (toText)
import Data.Functor ((<&>))
import Deriving.Aeson
  ( CamelToSnake,
    CustomJSON (CustomJSON),
    FieldLabelModifier,
    StripPrefix,
  )
import Extended.Text (Text)
import Extended.Text qualified as T
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary.Generic
  ( GenericArbitrary (GenericArbitrary),
  )

data User

type Key = T.Text

type Server = T.Text

type Ts = Int

type ErrorCode = Int

type FrontUser = ID User

data FrontDataResponse = FrontDataResponse
  { key :: !Key,
    server :: !Server,
    ts :: !Ts
  }
  deriving (Show, Generic, Eq, ToJSON)

instance FromJSON FrontDataResponse where
  parseJSON = withObject "FirstResponse VK" $ \v -> do
    r <- v .: "response"
    key <- r .: "key"
    server <- r .: "server"
    Right ts <- r .: "ts" <&> T.readEither
    pure $ FrontDataResponse {..}

data GoodResponse = GoodResponse {goodTs :: !Text, updates :: [Update]}
  deriving (Show, Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "good", CamelToSnake]] GoodResponse

data Update
  = EchoUpdate !T.Text (ID User) [Attachment]
  | HelpUpdate !(ID User)
  | RepeatUpdate !(ID User)
  | UpdateRepeats !(ID User) !Repeat
  | Trash !T.Text
  deriving (Show, Generic)
  deriving (Arbitrary) via GenericArbitrary Update

instance FromJSON Update where
  parseJSON = withObject "Update" $ \v -> do
    t <- v .: "type"
    case t of
      "message_new" -> do
        Message {..} <- v .: "object"
        pure $ case text of
          "/help" -> HelpUpdate from_id
          "/repeat" -> RepeatUpdate from_id
          other -> EchoUpdate other from_id attachments
      "message_event" -> do
        o <- v .: "object"
        userID <- o .: "user_id"
        payload <- o .: "payload"
        pure $ UpdateRepeats userID payload
      _ -> pure $ Trash t

data Message = Message
  { from_id :: !(ID User),
    text :: !Text,
    fwd_messages :: [Object],
    attachments :: [Attachment]
  }
  deriving (Show, Generic, FromJSON)
  deriving (Arbitrary) via GenericArbitrary Message

data Attachment = Attachment
  { _type :: !T.Text,
    _id :: !(ID Attachment),
    owner :: !(Maybe (ID User)),
    acessKey :: !(Maybe T.Text)
  }
  deriving (Show, Generic)
  deriving (Arbitrary) via GenericArbitrary Attachment

instance FromJSON Attachment where
  parseJSON = withObject "VK_Attachment" $ \v -> do
    t <- v .: "type"
    let _type = toText t
    inner <- v .: t
    _id <- inner .: "sticker_id" <|> inner .: "id"
    owner <- inner .:? "owner_id"
    acessKey <- inner .:? "access_key"
    pure $ Attachment {..}

data BadResponse = BadResponse {failed :: !ErrorCode, badTs :: !(Maybe Ts)}
  deriving (Show, Generic, Eq)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "bad", CamelToSnake]] BadResponse

data Callback
  = GoodCallback Int
  | BadCallback
  deriving (Eq, Show, Generic)

instance FromJSON Callback where
  parseJSON = withObject "Callback" $ \v ->
    do
      GoodCallback <$> v .: "response"
      <|> pure BadCallback
