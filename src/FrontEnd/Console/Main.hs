module FrontEnd.Console.Main where

import Bot.Error (parse)
import Bot.FrontEnd
  ( Action (..),
    BotIO (NonWeb),
    HasEnv (getFrontEnv, getHelpMessage, getRepeatMessage, setFrontEnv),
    IsFrontEnd (..),
  )
import Control.Lens ((%~), (<&>))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.String (IsString (fromString))
import Extended.Text (Text)
import Extended.Text qualified as T
import FrontEnd.Console.Env
  ( ConsoleEnv (..),
    consoleAwaitsRepUpdate,
  )

data Console

data NotRequired = NotRequired deriving (Show, Eq, Ord)

instance IsFrontEnd Console where
  type BotIOType Console = 'NonWeb
  type BotUser Console = NotRequired
  type BotConfig Console = NotRequired
  type BotFrontEnv Console = ConsoleEnv
  mkFrontEnv _ = pure $ ConsoleEnv False
  type Update Console = Text
  getActions = chooseAction

getConsoleUpdates :: MonadIO m => m [Text]
getConsoleUpdates = pure <$> liftIO T.getLine

sendConsoleResponse :: MonadIO m => Text -> m ()
sendConsoleResponse = liftIO . T.putStrLn . ("λ:" <>)

chooseAction ::
  (Monad m, HasEnv Console m, MonadThrow m) =>
  Update Console ->
  m [Action Console]
chooseAction u =
  ifM
    consoleAwaitsNewNumberOfRepeatitions
    (getNewReps u)
    (getAction u)

consoleAwaitsNewNumberOfRepeatitions :: (HasEnv Console m, Functor m) => m Bool
consoleAwaitsNewNumberOfRepeatitions = getFrontEnv <&> _consoleAwaitsRepUpdate

getAction ::
  (Monad m, HasEnv Console m, MonadThrow m) =>
  Update Console ->
  m [Action Console]
getAction = \case
  "/help" -> pure . SendHelpMessage NotRequired <$> getHelpMessage
  "/repeat" -> flipMode >> (pure . SendRepeatMessage NotRequired <$> getRepeatMessage)
  text -> pure $ pure $ SendEcho NotRequired text text

getNewReps ::
  (Monad m, HasEnv Console m, MonadThrow m) =>
  Update Console ->
  m [Action Console]
getNewReps update =
  flipMode
    >> parse (fromString $ T.unpack update)
    <&> (pure . UpdateRepeats NotRequired)

flipMode :: (HasEnv Console m, Monad m) => m ()
flipMode = setFrontEnv $ consoleAwaitsRepUpdate %~ not
