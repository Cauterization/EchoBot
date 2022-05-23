{-# LANGUAGE DataKinds #-}

module Bot.FrontEnd where

import App.Config (Config)
import Bot.Types (Repeat (unRepeat), URL)
import Control.Monad.Catch (MonadThrow)
import Data.Kind (Constraint, Type)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Typeable (Proxy (..), Typeable, typeOf)
import Deriving.Aeson (Generic)
import Extended.HTTP qualified as HTTP
import Extended.Text (Text)
import Logger qualified
import Wait (MonadWait)

newtype FrontName f = FrontName Text
  deriving (Generic, Show)

frontName :: forall f s. (Typeable f, IsString s) => s
frontName =
  let fullName = show (typeOf (Proxy @f))
   in fromString $ fromMaybe fullName $ L.stripPrefix "Proxy * " fullName

data BotIO = NonWeb | Web

type family IORequirements a m :: Constraint where
  IORequirements 'NonWeb m = ()
  IORequirements 'Web m = HTTP.MonadHttp m

class (Show (BotUser f), Ord (BotUser f), Show (BotFrontEnv f), Typeable f) => IsFrontEnd f where
  -- | Key of bot's map with repeatitions
  type BotUser f :: Type

  type BotConfig f :: Type

  type BotFrontEnv f :: Type

  type BotIOType f :: BotIO

  mkFrontEnv :: (Monad m, MonadThrow m, HTTP.MonadHttp m, Logger.HasLogger m, MonadWait m) => Config -> m (BotFrontEnv f)

  --   mkEnv :: (Monad m, MonadThrow m) BotConfig f ->

  --   newFrontData ::
  --     (Monad m, MonadThrow m, HTTP.MonadHttp m) =>
  --     WebOnly f (Token f) ->
  --     m (FrontData f)

  -- | Updates that make up the front-end response
  type Update f :: Type

  getActions ::
    (Monad m, Logger.HasLogger m, MonadThrow m, HasEnv f m) =>
    Update f ->
    m [Action f]

--   -- | Function to parse each update
--   getActions ::
--     (Monad m, HasEnv f m, Logger.HasLogger m, HasEnv f m, MonadThrow m) =>
--     Update f ->
--     m [Action f]

--   prepareRequest :: (Monad m, HasEnv f m) => Update f -> m URL

-- | Bot environment seters and geters
class HasEnv f m | m -> f where
  getRepeats :: BotUser f -> m (Maybe Repeat)
  setRepeats :: BotUser f -> Repeat -> m ()
  defaultRepeats :: m Repeat
  getFrontEnv :: m (BotFrontEnv f)
  setFrontEnv :: (BotFrontEnv f -> BotFrontEnv f) -> m ()
  getHelpMessage :: m Text
  getRepeatMessage :: m Text

getRepeatsFor :: forall f m. (HasEnv f m, Monad m) => BotUser f -> m Int
getRepeatsFor u = getRepeats @f u >>= fmap unRepeat . maybe (defaultRepeats @f) pure

-- -- | Wee need that thing because of vkontakte's partial frontEnd data update
-- updateFrontData ::
--   forall f m.
--   (HasEnv f m, Monad m, Semigroup (FrontData f)) =>
--   FrontData f ->
--   m ()
-- updateFrontData fd = getFrontData @f >>= setFrontData @f . (fd <>)

-- | Bot actions
data Action f
  = SendEcho (BotUser f) URL Text
  | SendRepeatEcho (BotUser f) URL Text
  | SendHelpMessage (BotUser f) URL
  | SendRepeatMessage (BotUser f) URL
  | UpdateRepeats (BotUser f) Repeat
  | HideKeyboard (BotUser f) URL

-- deriving instance (Show (BotUser f), Show (WebOnly f URL)) => Show (Action f)

-- deriving instance (Eq (BotUser f), Eq (WebOnly f URL)) => Eq (Action f)

-- -- | Class for web front-end only
-- class
--   ( WebOnly f (Token f) ~ Token f,
--     WebOnly f (FrontData f) ~ FrontData f,
--     Semigroup (FrontData f),
--     WebOnly f PollingTime ~ PollingTime,
--     WebOnly f Text ~ Text,
--     HasEnv f m
--   ) =>
--   IsWebFrontEnd m f
--   where
--   -- | A front-end response consisting of updates and front-end data
--   type Response f :: Type

--   extractFrontData :: Response f -> FrontData f

--   extractUpdates :: Response f -> [Update f]

--   -- | URL for geting updates
--   getUpdatesURL :: Token f -> FrontData f -> PollingTime -> URL

--   -- | A response received during errors
--   type BadResponse f :: Type

--   handleBadResponse :: BadResponse f -> m ()

--   checkCallback :: BL.ByteString -> m () -}
