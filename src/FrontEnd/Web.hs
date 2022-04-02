{-# LANGUAGE FunctionalDependencies #-}
module FrontEnd.Web where

import Control.Monad
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Aeson ( withText, FromJSON(parseJSON), Value(String), GFromJSON, Zero, eitherDecode )
import Data.Kind (Type, Constraint)
-- import Data.Typeable (Typeable, typeOf, Proxy (..))

-- import Extended.Text (Text)
import Extended.Text qualified as T
import Extended.HTTP qualified as HTTP

-- import GHC.Generics (Generic (Rep))

import Bot.Error
import Bot.Types

-- import Data.Functor
-- import qualified Console.FrontEnd as Console
-- import Deriving.Aeson


import FrontEnd.FrontEnd
import Data.Functor ((<&>))

-- data Action (f :: FrontEnd)
--     = SendEcho (SendEcho f)
--     | SendHelp (SendHelp f)
--     | UpdateRepeats (User f) Repeat
--     | SendKeyboard (WebOnly f (SendKeyboard f))
--     | HideKeyboard (HideKeyboard f)

-- class FrontEndIO (f :: FrontEnd) (m :: Type -> Type) where

--     mkRequest :: m (Response f)

-- instance {-# OVERLAPPABLE #-}
--     ( IsFrontEnd f
--     , Monad m
--     , MonadThrow m
--     , HTTP.MonadHttp m
--     , MonadThrow m
--     , HasWebEnv f m
--     , FromJSON (Response f)
--     , FromJSON (BadResponse f)
--     ) 
--     => FrontEndIO (f :: FrontEnd) m where
--     mkRequest = 
--         getUpdatesURL @f <$> getToken <*> getFrontData <*> getPollingTime 
--         >>= HTTP.tryRequest 
--         >>= \x -> case eitherDecode @(Response f) x of
--             Right r -> pure r
--             Left _ -> parse @(BadResponse f) x >>= handleBadResponse @f

-- class ( IsWebFrontEnd f 
--       ) => HasWebEnv (f :: FrontEnd) m | m -> f where
--     getFrontData   :: m (FrontData f)
--     setFrontData   :: FrontData f -> m ()
--     getToken       :: m (Token f)
--     getPollingTime :: m PollingTime

-- class ( WebOnly f URL ~ URL
--       , WebOnly f (Token f) ~ Token f
--       , WebOnly f (FrontData f) ~ FrontData f
--       , WebOnly f PollingTime ~ PollingTime
--       ) => IsWebFrontEnd (f :: FrontEnd) where

--     getUpdatesURL :: Token f -> FrontData f -> PollingTime -> URL

--     type BadResponse f :: Type

--     handleBadResponse :: BadResponse f -> m a
