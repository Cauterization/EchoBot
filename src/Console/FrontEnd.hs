module Console.FrontEnd where

import Bot.Error ( parse )
import Bot.FrontEnd
    ( NotRequired(..),
      HasEnv(getRepeatMessage, getFrontData, setFrontData,
             getHelpMessage),
      IsFrontEnd(..),
      Action(UpdateRepeats, SendHelpMessage, SendRepeatMessage,
             SendRepeatEcho) )
import Control.Monad.Catch ( MonadThrow )
import Control.Monad.Extra ( ifM )
import Data.Aeson ( FromJSON )
import Data.Functor ( (<&>) )
import Data.Monoid ( Any(Any) )
import Data.String ( IsString(fromString) )
import Extended.Text (Text)
import Extended.Text qualified as T
import GHC.Generics ( Generic )

data Console = Console deriving (Generic, FromJSON, Show)

newtype ConsoleAwaits = ConsoleAwaits {unAwaits :: Bool}
  deriving (Semigroup, Monoid) via Any

instance IsFrontEnd Console where
  type WebOnly Console _ = NotRequired

  type BotUser Console = NotRequired

  type Update Console = Text

  type FrontData Console = ConsoleAwaits

  newFrontData _ = pure mempty

  getActions = chooseAction

  prepareRequest = pure

consoleAwaitsNewNumberOfRepeatitions :: (HasEnv Console m, Functor m) => m Bool
consoleAwaitsNewNumberOfRepeatitions = getFrontData <&> unAwaits

flipMode :: (HasEnv Console m, Monad m) => m ()
flipMode = getFrontData >>= setFrontData . ConsoleAwaits . not . unAwaits

chooseAction ::
  (Monad m, HasEnv Console m, MonadThrow m) =>
  Update Console ->
  m [Action Console]
chooseAction u =
  ifM
    consoleAwaitsNewNumberOfRepeatitions
    (getNewReps u)
    (getAction u)

getAction ::
  (Monad m, HasEnv Console m, MonadThrow m) =>
  Update Console ->
  m [Action Console]
getAction = \case
  "/help" -> pure . SendHelpMessage NotRequired <$> getHelpMessage
  "/repeat" -> flipMode >> (pure . SendRepeatMessage NotRequired <$> getRepeatMessage)
  text -> pure $ pure $ SendRepeatEcho NotRequired text text

getNewReps ::
  (Monad m, HasEnv Console m, MonadThrow m) =>
  Update Console ->
  m [Action Console]
getNewReps update =
  flipMode
    >> parse (fromString $ T.unpack update)
    <&> (pure . UpdateRepeats NotRequired)
