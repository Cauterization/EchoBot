{-# LANGUAGE FunctionalDependencies #-}

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


frontName :: forall f s. (Typeable f, IsString s) => s
frontName = 
    let fullName = show (typeOf (Proxy @f))
    in fromString $ fromMaybe fullName $ L.stripPrefix "Proxy * " fullName

newtype Token f = Token {unToken :: Text}
    deriving (Show)

instance Typeable f => FromJSON (Token f) where
    parseJSON = withObject "Token" $ \v -> Token <$> v .: (frontName @f)

data NotRequired = NotRequired deriving (Show, Eq, Ord)

instance FromJSON NotRequired where
  parseJSON _ = pure NotRequired

newtype FrontName f = FrontName Text
    deriving (Generic, Show)

instance (Typeable f) => FromJSON (FrontName f) where
    parseJSON = withText "FrontEnd" $ \t -> do
        guard $ "Proxy * " <> t == T.show (typeOf (Proxy @f))
        pure $ FrontName t

class Ord (BotUser f) => IsFrontEnd f where 

    type family WebOnly f a :: Type

    type BotUser f :: Type

    type FrontData f :: Type 

    newFrontData :: (Monad m, MonadThrow m, HTTP.MonadHttp m) 
         => WebOnly f (Token f) -> m (FrontData f)

    type Update f :: Type

    getActions :: (Monad m, HasEnv f m, Logger.HasLogger m, HasEnv f m, MonadThrow m) 
        => Update f -> m [Action f]

class HasEnv f m | m -> f where
    getRepeats       :: BotUser f -> m (Maybe Repeat)
    setRepeats       :: BotUser f -> Repeat -> m ()
    defaultRepeats   :: m Repeat
    getFrontData     :: m (FrontData f)
    setFrontData     :: FrontData f -> m ()
    getToken         :: m (WebOnly f (Token f))
    getPollingTime   :: m (WebOnly f PollingTime)
    getHelpMessage   :: m Text
    getRepeatMessage :: m Text

getRepeatsFor :: forall f m. (HasEnv f m, Monad m) => BotUser f -> m Int
getRepeatsFor u = getRepeats u >>= fmap unRepeat . maybe defaultRepeats pure

-- | Wee need that thing because of vkontakte partial frontEnd data update 
updateFrontData :: forall f m. (HasEnv f m, Monad m, Semigroup (FrontData f)) 
    => FrontData f -> m ()
updateFrontData fd = getFrontData >>= setFrontData . (fd <>)

data Action f 
    = SendEcho URL
    | SendRepeatEcho (BotUser f) URL 
    | UpdateRepeats (BotUser f) Repeat
    | SendKeyboard (WebOnly f URL)
    | HideKeyboard (WebOnly f URL)

class ( WebOnly f (Token f) ~ Token f
      , WebOnly f (FrontData f) ~ FrontData f
      , Semigroup (FrontData f)
      , WebOnly f PollingTime ~ PollingTime
      , WebOnly f Text ~ Text
      , HasEnv f m
      ) => IsWebFrontEnd m f where

    type Response f :: Type

    extractFrontData :: Response f -> FrontData f

    extractUpdates :: Response f -> [Update f]
          
    getUpdatesURL :: Token f -> FrontData f -> PollingTime -> URL

    type BadResponse f :: Type

    handleBadResponse :: BadResponse f -> m ()

    checkCallback :: BL.ByteString -> m ()

