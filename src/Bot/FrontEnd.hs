{-# LANGUAGE DeriveAnyClass #-}

module Bot.FrontEnd where
    
import Control.Monad (guard)

import Data.Aeson ( withText, FromJSON(parseJSON), Value(String) )
import Data.Typeable (Typeable, typeOf, Proxy (..))

import Extended.Text qualified as T

import GHC.Generics (Generic)

data FrontEnd = Vkontakte | Telegram | Console deriving (Show, Generic, FromJSON)

data NotRequired = NotRequired deriving Show

instance FromJSON NotRequired where
  parseJSON _ = pure NotRequired

newtype FrontName (f :: FrontEnd) = FrontName FrontEnd
    deriving (Generic)
    deriving newtype (Show)

instance Typeable f => FromJSON (FrontName f) where
    parseJSON = withText "FrontEnd" $ \t -> do
        guard $ "Proxy FrontEnd '" <> t == T.show (typeOf (Proxy @f))
        FrontName <$> parseJSON (String t)

type family WebField (f :: FrontEnd) a where
    WebField 'Console a = NotRequired
    WebField f        a = a
