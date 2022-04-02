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
      StripPrefix )
import qualified Logger.Handle as Logger

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
        CustomJSON '[ FieldLabelModifier (StripPrefix "good") -- CamelToSnake
                    ] GoodResponse


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
        CustomJSON '[ FieldLabelModifier (StripPrefix "bad") -- CamelToSnake
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