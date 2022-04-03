{-# LANGUAGE ViewPatterns #-}
module Console.FrontEnd where


import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.IO.Class

import Data.Aeson
import Data.String

import Extended.Text (Text)
import Extended.Text qualified as T

import Bot.Error
import Bot.FrontEnd
import Bot.IO

import GHC.Generics


data Console = Console deriving (Generic, FromJSON, Show)

instance IsFrontEnd Console where

    type WebOnly   Console _ = NotRequired

    type BotUser   Console = NotRequired

    type Update    Console = Text

    -- | Is bot awaits number of repeatitions?
    type FrontData Console = Bool

    newFrontData _ = pure False

    getActions = chooseAction

consoleAwaitsNewNumberOfRepeatitions :: HasEnv Console m => m (FrontData Console)
consoleAwaitsNewNumberOfRepeatitions = getFrontData

flipMode :: (HasEnv Console m, Monad m) => m ()
flipMode = getFrontData >>= setFrontData . not

chooseAction :: (Monad m, HasEnv Console m, MonadThrow m) 
    => Update Console -> m [Action Console]
chooseAction u = ifM consoleAwaitsNewNumberOfRepeatitions (getNewReps u) (getAction u)

getAction :: (Monad m, HasEnv Console m, MonadThrow m) 
    => Update Console -> m [Action Console]
getAction = \case 
    "/help"   -> pure . SendHelpMessage <$> getHelpMessage

    "/repeat" -> flipMode >> (pure . SendRepeatMessage <$> getRepeatMessage )

    text      -> pure $ pure $ SendRepeatEcho NotRequired text

getNewReps :: (Monad m, HasEnv Console m, MonadThrow m) 
    => Update Console -> m [Action Console]
getNewReps update
    = flipMode >> either 
        (throwM . ParsingError . T.pack)
        (pure . pure . UpdateRepeats NotRequired) 
        (eitherDecode . fromString $ T.unpack update)
        
instance {-# OVERLAPPING #-} MonadIO m => FrontEndIO Console m where
    
    getUpdates = pure <$> liftIO T.getLine

    sendResponse = liftIO . T.putStrLn . ("λ:" <>)

    sendWebResponse _ = pure ()