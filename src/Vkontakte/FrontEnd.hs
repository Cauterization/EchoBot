{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
      StripPrefix )
import Data.Traversable

data User
type Key       = T.Text
type Server    = T.Text
type Ts        = Int
type ErrorCode = Int

type FrontUser = ID User

data ConnectionData = ConnectionData
    { key    :: !Key
    , server :: !Server
    , ts     :: !Ts
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

getUpdatesURL :: Token (f :: FrontEnd) -> ConnectionData -> PollingTime -> URL
getUpdatesURL (Token t) ConnectionData{..} polling = mconcat 
    [ server 
    , "?act=a_check&key=", key
    , "&ts=", T.show ts
    , "&wait=", T.show polling
    ]

newConnectionData :: (Monad m, HTTP.MonadHttp m, MonadThrow m) 
    => Token f -> m ConnectionData
newConnectionData (Token t) = do
    let req = "https://api.vk.com/method/groups.getLongPollServer"
           <> "?group_id=204518764"
           <> "&access_token=" <> t
           <> "&v=5.81"
    HTTP.tryRequest req >>= parse @FirstResponse <&> response

newtype FirstResponse = FirstResponse{response :: ConnectionData}
instance FromJSON FirstResponse where
    parseJSON = withObject "FirstResponse" $ \v -> do
        r        <- v .: "response"
        key      <- r .: "key"
        server   <- r .: "server"
        Right ts <- r .: "ts" <&> T.readEither
        pure $ FirstResponse ConnectionData{..}

data GoodResponse = GoodResponse { ts :: !Text, updates :: [Update]}
    deriving (Show, Generic, FromJSON)

data Update = Update        !Message
            | UpdateRepeats !(ID User)  !Repeat
            | Trash         !T.Text
            deriving (Show, Generic, FromJSON)

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
        t   <- v .: "type"
        let _type = T.show t
        inner   <- v .: t
        _id <- inner .:  "sticker_id" <|> inner .: "id"
        owner  <- inner .:  "owner_id"   <|> inner .: "from_id"
        acessKey    <- inner .:? "access_key"
        pure $ Attachment{..}

data BadResponse = BadResponse {failed :: !ErrorCode, brTs :: !(Maybe Ts)} 
    deriving (Show, Generic, Eq)
    deriving (FromJSON, ToJSON) via 
        CustomJSON '[ FieldLabelModifier CamelToSnake
                    , FieldLabelModifier (StripPrefix "br")
                    ] BadResponse

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