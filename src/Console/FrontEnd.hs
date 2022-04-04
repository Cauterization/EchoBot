{-# LANGUAGE ViewPatterns #-}
module Console.FrontEnd where


import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.IO.Class

import Data.Aeson
import Data.Functor
import Data.Monoid
import Data.String

import Extended.Text (Text)
import Extended.Text qualified as T

import GHC.Generics

import Bot.Error
import Bot.FrontEnd
import Bot.IO

data Console = Console deriving (Generic, FromJSON, Show)

newtype ConsoleAwaits = ConsoleAwaits {unAwaits :: Bool}
    deriving (Semigroup, Monoid) via Any

instance IsFrontEnd Console where

    type WebOnly   Console _ = NotRequired

    type BotUser   Console = NotRequired

    type Update    Console = Text

    -- | Is bot awaits number of repeatitions?
    type FrontData Console = ConsoleAwaits

    newFrontData _ = pure mempty

    getActions = chooseAction

consoleAwaitsNewNumberOfRepeatitions :: (HasEnv Console m, Functor m) => m Bool
consoleAwaitsNewNumberOfRepeatitions = getFrontData <&> unAwaits

flipMode :: (HasEnv Console m, Monad m) => m ()
flipMode = getFrontData >>= setFrontData . ConsoleAwaits . not . unAwaits

chooseAction :: (Monad m, HasEnv Console m, MonadThrow m) 
    => Update Console -> m [Action Console]
chooseAction u = ifM consoleAwaitsNewNumberOfRepeatitions 
    (getNewReps u) (getAction u)

getAction :: (Monad m, HasEnv Console m, MonadThrow m) 
    => Update Console -> m [Action Console]
getAction = \case 
    "/help"   -> pure . SendHelpMessage NotRequired <$> getHelpMessage

    "/repeat" -> flipMode >> (pure . SendRepeatMessage NotRequired <$> getRepeatMessage )

    text      -> pure $ pure $ SendRepeatEcho NotRequired text text

getNewReps :: (Monad m, HasEnv Console m, MonadThrow m) 
    => Update Console -> m [Action Console]
getNewReps update = flipMode 
    >> parse (fromString $ T.unpack update) 
    <&> (pure . UpdateRepeats NotRequired)


instance {-# OVERLAPPING #-} MonadIO m => FrontEndIO Console m where
    
    getUpdates = pure <$> liftIO T.getLine

    sendResponse = liftIO . T.putStrLn . ("λ:" <>)

    sendWebResponse _ = pure ()