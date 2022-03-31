{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.Types where

import Control.Applicative ((<|>), Alternative)
import Control.Monad (guard, unless)

import Data.Aeson (FromJSON, parseJSON, withText, withObject, Value (Object, String), (.:), eitherDecode)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))

import Extended.Text (Text)
import Extended.Text qualified as T

import Data.Typeable (Typeable, typeOf, Proxy (Proxy))

import GHC.Generics (Generic)

newtype ID e = ID { idVal :: Int }
  deriving stock (Show, Generic)
  deriving newtype (Read)

newtype Repeat = Repeat Int deriving (Generic, Show)

instance FromJSON Repeat where
    parseJSON r = do
        x <- parseJSON @Int r
        unless (x `elem` [1..5]) $ fail "incorrect number of default repeatitions"
        pure $ Repeat x

type URL = Text

type PollingTime = Int

data FrontEnd = Vkontakte | Telegram | Console 
    deriving (Show, Generic, FromJSON)

frontName :: forall (f :: FrontEnd) s. (Typeable f, IsString s) => s
frontName = 
    let fullName = show (typeOf (Proxy @f))
    in fromString $ fromMaybe fullName $ L.stripPrefix "Proxy FrontEnd '" fullName

newtype Token (f :: FrontEnd ) = Token Text 
    deriving newtype (Show)

instance Typeable f => FromJSON (Token f) where
    parseJSON = withObject "Token" $ \v -> Token <$> v .: (frontName @f)

class HasToken f m | m -> f where
    getToken :: m (Token f)

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

