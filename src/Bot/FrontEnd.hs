{-# LANGUAGE GADTs  #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.FrontEnd where

import Control.Monad (guard, (>=>), liftM2, join)
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

    getActions :: (Monad m, HasEnv f m, Logger.HasLogger m) => Update f -> m [Action f]

class HasEnv f m | m -> f where
    getRepeats       :: User f -> m (Maybe Repeat)
    setRepeats       :: User f -> Repeat -> m ()
    defaultRepeats   :: m Repeat
    getFrontData     :: m (FrontData f)
    setFrontData     :: FrontData f -> m ()
    getToken         :: m (WebOnly f (Token f))
    getPollingTime   :: m (WebOnly f PollingTime)
    getHelpMessage   :: m Text
    getRepeatMessage :: m Text

getRepeatsFor :: forall f m. (HasEnv f m, Monad m) => User f -> m Int
getRepeatsFor u = getRepeats u >>= fmap unRepeat . maybe defaultRepeats pure

-- | Wee need it because of vkontakte partial frontEnd data update 
updateFrontData :: forall f m. (HasEnv f m, Monad m, Semigroup (FrontData f)) => 
    FrontData f -> m ()
updateFrontData fd = do
    oldFD <- getFrontData
    setFrontData $ fd <> oldFD

data Action f 
    = SendEcho URL
    | SendRepeatEcho (User f) URL 
    | UpdateRepeats (User f) Repeat
    | SendKeyboard (WebOnly f URL)
    | HideKeyboard (WebOnly f URL)

class ( WebOnly f (Token f) ~ Token f
      , WebOnly f (FrontData f) ~ FrontData f
      , Semigroup (FrontData f)
      , WebOnly f PollingTime ~ PollingTime
      , WebOnly f Text ~ Text
      , HasEnv f m
      ) => IsWebFrontEnd f m where

    type Response f :: Type

    extractFrontData :: Response f -> FrontData f

    extractUpdates :: Response f -> [Update f]
          
    getUpdatesURL :: Token f -> FrontData f -> PollingTime -> URL

    type BadResponse f :: Type

    handleBadResponse :: BadResponse f -> m ()

    checkCallback :: BL.ByteString -> m ()

