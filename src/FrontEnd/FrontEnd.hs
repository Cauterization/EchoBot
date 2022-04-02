{-# LANGUAGE GADTs  #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FrontEnd.FrontEnd where
    
import Control.Arrow

import Control.Monad (guard, (>=>), liftM2)
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Aeson 
import Data.Kind (Type, Constraint)
import Data.List qualified as L
import Data.Maybe
import Data.String
import Data.Typeable (Typeable, typeOf, Proxy (..))

import Extended.Text (Text)
import Extended.Text qualified as T
import Extended.HTTP qualified as HTTP

import GHC.Generics (Generic (Rep))

import Bot.Error
import Bot.Types

import Deriving.Aeson
import Logger.Handle ((>.))
import qualified Logger.Handle as Logger
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Data.Functor

-- data FrontEnd 
--     = Vkontakte 
--     -- | Telegram
--     | Console 
--     deriving (Show, Generic, FromJSON)

frontName :: forall f s. (Typeable f, IsString s) => s
frontName = 
    let fullName = show (typeOf (Proxy @f))
    in fromString $ fromMaybe fullName $ L.stripPrefix "Proxy * " fullName

newtype Token f = Token {unToken :: Text}
    deriving (Show)

instance Typeable f => FromJSON (Token f) where
    parseJSON = withObject "Token" $ \v -> Token <$> v .: (frontName @f)

    -- WebOnly 'Console a = NotRequired
    -- WebOnly f        a = a

data NotRequired = NotRequired deriving (Show, Eq, Ord)

instance FromJSON NotRequired where
  parseJSON _ = pure NotRequired

newtype FrontName f = FrontName Text
    deriving (Generic, Show)

instance (Typeable f) => FromJSON (FrontName f) where
    parseJSON = withText "FrontEnd" $ \t -> do
        guard $ "Proxy * " <> t == T.show (typeOf (Proxy @f))
        pure $ FrontName t

class Ord (User f) => IsFrontEnd f where 

    type family WebOnly f a :: Type

    type User f :: Type

    type FrontData f :: Type 

    newFrontData :: (Monad m, MonadThrow m, MonadIO m) 
         => WebOnly f (Token f) -> m (FrontData f)

    type Update f :: Type

    getActions :: (Monad m, HasWebEnv f m) => Update f -> m [Action f]

class HasRepeats m f | m -> f where
    getRepeats :: User f -> m (Maybe Repeat)
    setRepeats :: User f -> m ()
    defaultRepeats :: m Repeat

data Action f =
    SendEcho (User f) URL
    -- | SendHelp (SendHelp f)
    -- | UpdateRepeats (User f) Repeat
    -- | SendKeyboard (WebOnly f (SendKeyboard f))
    -- | HideKeyboard (HideKeyboard f)

class FrontEndIO f (m :: Type -> Type) | m -> f where

    getUpdates :: m [Update f]

    sendResponse :: Text -> m ()

instance {-# OVERLAPPABLE #-}
    ( Monad m
    , MonadThrow m
    , HTTP.MonadHttp m
    , Logger.HasLogger m
    , MonadThrow m
    , IsWebFrontEnd f m
    , FromJSON (Response f)
    , FromJSON (BadResponse f)
    , Show (Response f)
    ) 
    => FrontEndIO f m where

    getUpdates = 
        getUpdatesURL @f @m <$> getToken @f @m <*> getFrontData <*> getPollingTime 
        >>= HTTP.tryRequest 
        >>= \x -> case eitherDecode @(Response f) x of
            Left _ -> parse @(BadResponse f) x >>= fmap (fmap (const [])) handleBadResponse
            Right r -> do
                Logger.debug $ "Recieved response:" Logger..< r
                setFrontData $ extractFrontData @f @m r 
                pure $ extractUpdates @f @m r

    sendResponse = HTTP.tryRequest >=> checkCallback @f

class HasWebEnv f m | m -> f where
    getFrontData   :: m (FrontData f)
    setFrontData   :: FrontData f -> m ()
    getToken       :: m (Token f)
    getPollingTime :: m PollingTime

class ( WebOnly f URL ~ URL
      , WebOnly f (Token f) ~ Token f
      , WebOnly f (FrontData f) ~ FrontData f
      , WebOnly f PollingTime ~ PollingTime
      , HasWebEnv f m
      ) => IsWebFrontEnd f m | m -> f where

    type Response f :: Type

    extractFrontData :: Response f -> FrontData f

    extractUpdates :: Response f -> [Update f]
          
    getUpdatesURL :: Token f -> FrontData f -> PollingTime -> URL

    type BadResponse f :: Type

    handleBadResponse :: BadResponse f -> m ()

    checkCallback :: BL.ByteString -> m ()

