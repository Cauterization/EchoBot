{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE ViewPatterns #-}

module FrontEnd.FrontEnd where
    
import Control.Monad (guard, (>=>))
import Control.Monad.Catch

import Data.Aeson ( withText, FromJSON(parseJSON), Value(String), GFromJSON, Zero )
import Data.Kind (Type, Constraint)
import Data.Typeable (Typeable, typeOf, Proxy (..))

import Extended.Text (Text)
import Extended.Text qualified as T
import Extended.HTTP qualified as HTTP

import GHC.Generics (Generic (Rep))

import Bot.Error
import Bot.Types

import Vkontakte.FrontEnd qualified as VK
import FrontEnd.Web
import Control.Applicative
import Control.Monad.IO.Class
import Data.Functor
import qualified Console.FrontEnd as Console
import Deriving.Aeson

type family WebOnly (f :: FrontEnd) a where
    WebOnly 'Console a = NotRequired
    WebOnly f        a = a

class (Show (Response f)) => IsFrontEnd (f :: FrontEnd) where 

    type User f :: Type

    type ConnectionData f :: Type

    newConnectionData :: forall m. (Monad m, MonadThrow m) 
        => WebOnly f (Token f) -> IO (ConnectionData f)

    type Response f :: Type

    -- type BadResponse f :: Type

    type SendHelp f :: Type
    type SendKeyboard f :: Type
    type UpdateRepeats f :: Type
    type HideKeyboard f :: Type

    getActions :: Response f -> [Action f]

-- data Response (f :: FrontEnd) 
--     = GoodResponse (GoodResponse f) 
--     | BadResponse (BadResponse f) 
--     deriving Generic
-- deriving via CustomJSON '[ SumUntaggedValue ] (Response f) 
--     instance (FromJSON (GoodResponse f), FromJSON (BadResponse f)) 
--         => FromJSON (Response f) 

-- instance (FromJSON (GoodResponse f), FromJSON (BadResponse f), Generic (Response f)) 
--     => (FromJSON (Response (f :: FrontEnd))) where
--     parseJSON v = GoodResponse <$> parseJSON @(GoodResponse f) v
--               <|> BadResponse  <$> parseJSON @(BadResponse  f) v

data Action (f :: FrontEnd) where
    SendHelp      :: SendHelp f -> Action f
    SendKeyboard  :: SendKeyboard f -> Action f
    UpdateRepeats :: UpdateRepeats f -> Action f
    HideKeyboard  :: HideKeyboard f -> Action f
        
-- data FrontEndError f = BadResponseErr (BadResponse f) 
-- deriving instance Show (BadResponse f) => Show (FrontEndError f)
-- deriving instance (Show (BadResponse f), Typeable f) => Exception (FrontEndError f)

-- data Update f 
--     = SendEcho         (SendEcho f)
--     | SendHelp         (SendHelp f)
--     | SendKeyboard (SendKeyboard f)
--     | HideKeyboard (HideKeyboard f)
--     | DoNothing 
    
-- instance ( Show (SendEcho f)
--          , Show (SendHelp f)
--          , Show (SendKeyboard f)
--          , Show (HideKeyboard f)
--          ) => Show (Update f) where
--     show = \case
--         SendEcho s -> show s
--         SendHelp s ->  show s
--         SendKeyboard s ->  show s
--         HideKeyboard s ->  show s
--         DoNothing -> "DoNothing"

instance IsFrontEnd 'Console where

    type User 'Console = ()

    type ConnectionData 'Console = NotRequired

    newConnectionData _ = pure NotRequired

    type Response 'Console = Text

class ( WebOnly f URL ~ URL
      , WebOnly f (Token f) ~ Token f
      , WebOnly f (ConnectionData f) ~ ConnectionData f
      , WebOnly f PollingTime ~ PollingTime) 
      => IsWebFrontEnd (f :: FrontEnd) where

    getUpdatesURL :: Token f -> ConnectionData f -> PollingTime -> URL

    type BadResponse f :: Type

    -- type BadResponse 'Console = NotRequired

    -- type SendEcho     'Console = NotRequired
    -- type SendHelp     'Console = NotRequired
    -- type SendKeyboard 'Console = NotRequired
    -- type HideKeyboard 'Console = NotRequired

instance IsFrontEnd 'Vkontakte where

    type User 'Vkontakte = VK.FrontUser

    type ConnectionData 'Vkontakte = VK.ConnectionData 

    newConnectionData = VK.newConnectionData

    type Response 'Vkontakte = VK.GoodResponse

instance IsWebFrontEnd 'Vkontakte where

    getUpdatesURL = VK.getUpdatesURL

    type BadResponse 'Vkontakte = VK.BadResponse

    -- 

    -- type Command 'Vkontakte = VK.Command

    -- getActions = VK.getActions

-------------------------------------------------

-- instance IsFrontEnd 'Telegram where

--     type  GoodResponse 'Telegram = ()
--     type BadResponse 'Telegram = ()