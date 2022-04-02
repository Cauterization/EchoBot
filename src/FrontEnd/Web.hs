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

