module Vkontakte.Web where

-- import Data.Aeson hiding (Key)
import Control.Monad.Catch

import Data.Functor ( (<&>) )
-- import Extended.Text (Text)
import Extended.Text qualified as T

-- import Bot.Error
import Bot.Types

-- import Data.Traversable
import Extended.HTTP qualified as HTTP 

import qualified Logger.Handle as Logger

import FrontEnd.FrontEnd (IsFrontEnd, IsWebFrontEnd, FrontEnd(..), Token(..), Action)
import FrontEnd.FrontEnd qualified as Front

import Vkontakte.FrontEnd 

import Bot.Error
import Control.Monad.IO.Class (MonadIO)

instance IsFrontEnd 'Vkontakte where 

    type User      'Vkontakte = ID User

    type Update    'Vkontakte = Update

    type FrontData 'Vkontakte = FrontData

    newFrontData = newFrontData

    -- type SendEcho 'Vkontakte = User
    -- type SendHelp 'Vkontakte = User
    -- type SendKeyboard  f :: Type
    -- type HideKeyboard  f :: Type

    -- getActions :: GoodResponse -> [Action 'Vkontakte]
    getActions = undefined

instance IsWebFrontEnd 'Vkontakte where

    getUpdatesURL = getUpdatesURL

    type Response 'Vkontakte = GoodResponse 

    extractFrontData = fromTs . T.read . goodTs

    extractUpdates = updates

    type BadResponse 'Vkontakte = BadResponse

    handleBadResponse = handleBadResponse

    checkCallback = checkCallback

    -- newFrontData = newFrontData

newFrontData :: (Monad m, HTTP.MonadHttp m, MonadThrow m) 
    => Token f -> m FrontData
newFrontData (Token t) = do
    let req = "https://api.vk.com/method/groups.getLongPollServer"
           <> "?group_id=204518764"
           <> "&access_token=" <> t
           <> "&v=5.81"
    HTTP.tryRequest req >>= parse @FrontData

getUpdatesURL :: Token (f :: FrontEnd) -> FrontData -> PollingTime -> URL
getUpdatesURL _ FrontData{..} polling = mconcat 
    [ server 
    , "?act=a_check&key=", key
    , "&ts=", T.show ts
    , "&wait=", T.show polling
    ]

handleBadResponse :: 
    ( Monad m
    , MonadThrow m
    , HTTP.MonadHttp m
    , Logger.HasLogger m
    , Front.HasWebEnv 'Vkontakte m
    ) 
    => BadResponse -> m ()
handleBadResponse BadResponse{..} = case failed of
    1 -> Logger.warning "Update history is out of date. Updating TS... "
        >> maybe
            (Logger.error "Can't update TS - threre is no TS in this response!")
            (Front.setFrontData . fromTs)
            badTs
    2 -> Logger.warning "Key is out of date. Getting new key..."
        >> Front.getToken >>= newFrontData >>= Front.setFrontData
    3 -> Logger.warning "FrontEnd data is lost, requesting new one..."
        >> Front.getToken >>= newFrontData >>= Front.setFrontData @'Vkontakte
    _ -> Logger.error "Unknown error code. IDK what to do with this."

checkCallback = undefined
