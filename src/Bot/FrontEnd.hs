{-# LANGUAGE FunctionalDependencies #-}

module Bot.FrontEnd where

import Control.Monad (guard)
import Control.Monad.Catch

import Data.Aeson 
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.List qualified as L
import Data.Maybe
import Data.String
import Data.Typeable (Typeable, typeOf, Proxy (..))

import Extended.Text (Text)
import Extended.Text qualified as T
import Extended.HTTP qualified as HTTP

import Bot.Types

import Deriving.Aeson
import  Logger.Handle qualified as Logger

frontName :: forall f s. (Typeable f, IsString s) => s
frontName = 
    let fullName = show (typeOf (Proxy @f))
    in fromString $ fromMaybe fullName $ L.stripPrefix "Proxy * " fullName

newtype Token f = Token {unToken :: Text}
    deriving (Show)

instance Typeable f => FromJSON (Token f) where
    parseJSON = withObject "Token" $ \v -> Token <$> v .: (frontName @f)

-- | Dummy data for non-web front-end (console)
data NotRequired = NotRequired deriving (Show, Eq, Ord)

instance FromJSON NotRequired where
  parseJSON _ = pure NotRequired

newtype FrontName f = FrontName Text
    deriving (Generic, Show)

instance (Typeable f) => FromJSON (FrontName f) where
    parseJSON = withText "FrontEnd" $ \t -> do
        guard $ "Proxy * " <> t == T.show (typeOf (Proxy @f))
        pure $ FrontName t

class (Show (BotUser f), Monoid (FrontData f)) => IsFrontEnd f where 

    -- | Set NotRequired for no-web front-end or a otherwise
    type family WebOnly f a :: Type

    -- | Key of bot's map with repeatitions 
    type BotUser f :: Type

    -- | A bot stored data, specific for each front-end
    -- e.g. vkontakte server, key and ts
    -- telegram offset
    -- or console's flag for waiting of repeatitons
    type FrontData f :: Type 

    newFrontData :: (Monad m, MonadThrow m, HTTP.MonadHttp m) 
         => WebOnly f (Token f) -> m (FrontData f)

    -- | Updates that make up the front-end response
    type Update f :: Type

    -- | Function to parse each update 
    getActions :: (Monad m, HasEnv f m, Logger.HasLogger m, HasEnv f m, MonadThrow m) 
        => Update f -> m [Action f]

    prepareRequest :: (Monad m, HasEnv f m) => Update f -> m URL

-- | Bot environment seters and geters
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

-- | Bot actions
data Action f 
    = SendEcho          (BotUser f) URL Text
    | SendRepeatEcho    (BotUser f) URL Text
    | SendHelpMessage   (BotUser f) URL
    | SendRepeatMessage (BotUser f) URL
    | UpdateRepeats     (BotUser f) Repeat
    | HideKeyboard      (BotUser f) (WebOnly f URL)

deriving instance (Show (BotUser f), Show (WebOnly f URL)) => Show (Action f)
deriving instance (Eq (BotUser f), Eq (WebOnly f URL)) => Eq (Action f)

-- | Class for web front-end only
class ( WebOnly f (Token f) ~ Token f
      , WebOnly f (FrontData f) ~ FrontData f
      , Semigroup (FrontData f)
      , WebOnly f PollingTime ~ PollingTime
      , WebOnly f Text ~ Text
      , HasEnv f m
      ) => IsWebFrontEnd m f where

    -- | A front-end response consisting of updates and front-end data
    type Response f :: Type

    extractFrontData :: Response f -> FrontData f

    extractUpdates :: Response f -> [Update f]

    -- | URL for geting updates      
    getUpdatesURL :: Token f -> FrontData f -> PollingTime -> URL

    -- | A response received during errors
    type BadResponse f :: Type

    handleBadResponse :: BadResponse f -> m ()

    checkCallback :: BL.ByteString -> m ()

