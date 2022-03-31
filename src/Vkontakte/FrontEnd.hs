{-# LANGUAGE DeriveAnyClass #-}

module Vkontakte.FrontEnd where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:))

import Extended.Text qualified as T

import Bot.Error
import Bot.Types
import GHC.Generics (Generic)
import qualified Extended.HTTP as HTTP
import Control.Monad.Catch
import Data.Functor ((<&>))

data User
type Key       = T.Text
type Server    = T.Text
type Ts        = Int
type ErrorCode = Int

type FrontUser = ID User

type FrontUpdate = ()

data ConnectionData =  ConnectionData
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
           <> "&access_token=" <> T.unpack t
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