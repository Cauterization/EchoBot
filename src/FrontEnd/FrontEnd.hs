{-# LANGUAGE DeriveAnyClass #-}

module FrontEnd.FrontEnd where
    
import Control.Monad (guard)

import Data.Aeson ( withText, FromJSON(parseJSON), Value(String) )
import Data.Kind (Type)
import Data.Typeable (Typeable, typeOf, Proxy (..))

import Extended.Text (Text)
import Extended.Text qualified as T

import GHC.Generics (Generic)

import Bot.Types



type family WebField (f :: FrontEnd) a where
    WebField 'Console a = NotRequired
    WebField f        a = a

class IsFrontEnd (f :: FrontEnd) where 

    type Update f :: Type

    getUpdatesURL :: WebField f (Token f -> URL)


instance IsFrontEnd 'Console where

    type Update 'Console = Text

    getUpdatesURL = NotRequired
