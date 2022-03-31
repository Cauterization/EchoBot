{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE ViewPatterns #-}

module FrontEnd.FrontEnd where
    
import Control.Monad (guard, (>=>))
import Control.Monad.Catch

import Data.Aeson ( withText, FromJSON(parseJSON), Value(String) )
import Data.Kind (Type, Constraint)
import Data.Typeable (Typeable, typeOf, Proxy (..))

import Extended.Text (Text)
import Extended.Text qualified as T
import Extended.HTTP qualified as HTTP

import GHC.Generics (Generic)

import Bot.Error
import Bot.Types

import Vkontakte.FrontEnd qualified as VK
import Control.Applicative
import Control.Monad.IO.Class
import Data.Functor

type family WebOnly (f :: FrontEnd) a where
    WebOnly 'Console a = NotRequired
    WebOnly f        a = a

type family FrontConstraint (f :: FrontEnd) (m :: Type -> Type) :: Constraint where
    FrontConstraint 'Console _ = ()
    FrontConstraint f        m = (HTTP.MonadHttp m)

class (Show (BadResponse f), Typeable f) 
    => IsFrontEnd (f :: FrontEnd) where 

    type User f :: Type

    type ConnectionData f :: Type

    newConnectionData :: forall m. (Monad m, MonadThrow m) 
        => WebOnly f (Token f) -> IO (ConnectionData f)

    getUpdatesURL :: WebOnly f (Token f) 
                  -> WebOnly f (ConnectionData f) 
                  -> WebOnly f PollingTime 
                  -> WebOnly f URL

    type GoodResponse f :: Type

    type BadResponse f :: Type

    extractUpdates :: GoodResponse f -> [Update f]

data Response (f :: FrontEnd) 
    = GoodResponse (GoodResponse f)
    | BadResponse  (BadResponse  f)
    deriving Generic

instance (FromJSON (GoodResponse f), FromJSON (BadResponse f), Generic (Response f)) 
    => (FromJSON (Response (f :: FrontEnd))) where
    parseJSON v = GoodResponse <$> parseJSON @(GoodResponse f) v
              <|> BadResponse  <$> parseJSON @(BadResponse  f) v

class HasEnv (f :: FrontEnd) m | m -> f where
    getConnData    :: m (ConnectionData f)
    setConnData    :: ConnectionData f -> m ()
    getToken       :: m (WebOnly f (Token f))
    getPollingTime :: m (WebOnly f PollingTime)

data FrontEndError f = BadResponseErr (BadResponse f) 
deriving instance Show (BadResponse f) => Show (FrontEndError f)
deriving instance (Show (BadResponse f), Typeable f) => Exception (FrontEndError f)

data Update (f :: FrontEnd) = X () deriving Show

instance (Show (Update f)) => IsFrontEnd 'Console where

    type User 'Console = ()

    type ConnectionData 'Console = NotRequired

    newConnectionData _ = pure NotRequired

    getUpdatesURL _ _ _ = NotRequired

    type GoodResponse 'Console = Text

    type BadResponse 'Console = NotRequired

instance IsFrontEnd 'Vkontakte where

    type User 'Vkontakte = VK.FrontUser

    type ConnectionData 'Vkontakte = VK.ConnectionData 

    newConnectionData = VK.newConnectionData

    getUpdatesURL = VK.getUpdatesURL

    type GoodResponse 'Vkontakte = VK.GoodResponse

    type BadResponse 'Vkontakte = VK.BadResponse






-------------------------------------------------

instance IsFrontEnd 'Telegram where

    type GoodResponse 'Telegram = ()
    type BadResponse 'Telegram = ()
