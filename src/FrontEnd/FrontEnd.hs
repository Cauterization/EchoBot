{-# LANGUAGE DeriveAnyClass #-}

module FrontEnd.FrontEnd where
    
import Control.Monad (guard)
import Control.Monad.Catch

import Data.Aeson ( withText, FromJSON(parseJSON), Value(String) )
import Data.Kind (Type, Constraint)
import Data.Typeable (Typeable, typeOf, Proxy (..))

import Extended.Text (Text)
import Extended.Text qualified as T
import Extended.HTTP qualified as HTTP

import GHC.Generics (Generic)

import Bot.Types

import Vkontakte.FrontEnd qualified as VK

type family WebField (f :: FrontEnd) a where
    WebField 'Console a = NotRequired
    WebField f        a = a

type family FrontConstraint (f :: FrontEnd) (m :: * -> *) :: Constraint where
    FrontConstraint 'Console _ = ()
    FrontConstraint f        m = (HTTP.MonadHttp m, HasToken f m)

class IsFrontEnd (f :: FrontEnd) where 

    type Update f :: Type

    type User f :: Type

    type ConnectionData f :: *

    newConnectionData :: forall m. (Monad m, MonadThrow m) 
        => WebField f (Token f) -> IO (ConnectionData f)

    getUpdatesURL :: WebField f (Token f -> ConnectionData f -> PollingTime -> URL)

instance IsFrontEnd 'Console where

    type Update 'Console = Text

    type User 'Console = ()

    type ConnectionData 'Console = NotRequired

    newConnectionData _ = pure NotRequired

    getUpdatesURL = NotRequired

instance IsFrontEnd 'Vkontakte where

    type Update 'Vkontakte = VK.FrontUpdate

    type User 'Vkontakte = VK.FrontUser

    type ConnectionData 'Vkontakte = VK.ConnectionData 

    newConnectionData = VK.newConnectionData

    getUpdatesURL = VK.getUpdatesURL