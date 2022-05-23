{-# LANGUAGE DataKinds #-}

module Bot.FrontEnd where

import App.Config (Config)
import Bot.Types (Repeat (unRepeat))
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

  -- | Updates that make up the front-end response
  type Update f :: Type

  getActions ::
    (Monad m, Logger.HasLogger m, MonadThrow m, HasEnv f m) =>
    Update f ->
    m [Action f]

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

-- | Bot actions
data Action f
  = SendEcho (BotUser f) Text Text
  | SendRepeatEcho (BotUser f) Text Text
  | SendHelpMessage (BotUser f) Text
  | SendRepeatMessage (BotUser f) Text
  | UpdateRepeats (BotUser f) Repeat
  | HideKeyboard (BotUser f) Text

