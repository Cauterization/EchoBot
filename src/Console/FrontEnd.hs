{-# LANGUAGE MultiWayIf #-}
module Console.FrontEnd where

import Data.Text qualified as T

import Extended.Text (Text)
import Extended.Text qualified as T

import Bot.FrontEnd
import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics

import Bot.IO
import Control.Applicative
import Data.Function (on)
import Data.String
import Control.Monad.Catch
import Control.Monad.Extra

data Console = Console deriving (Generic, FromJSON, Show)

instance IsFrontEnd Console where

    type WebOnly   Console _ = NotRequired

    type BotUser   Console = NotRequired

    type Update    Console = Text

    -- | Is bot awaits number of repeatitions?
    type FrontData Console = Bool

    newFrontData _ = pure False

    getActions = getAction

getAction :: (Monad m, HasEnv Console m, MonadThrow m) => Update Console -> m [Action Console]
getAction = \case

    "/help"   -> pure . SendEcho <$> getHelpMessage

    "/repeat" -> setFrontData True >> liftA2 
        ((<>) `on` pure) 
        (SendEcho <$> getRepeatMessage) 
        (pure $ SendKeyboard NotRequired)

    text      -> fmap pure $ setFrontData False >> ifM getFrontData
        (pure $ either 
            (SendEcho . T.pack) 
            (UpdateRepeats NotRequired) 
            (eitherDecode (fromString $ T.unpack text)))
        (pure $ SendRepeatEcho NotRequired text)

instance {-# OVERLAPPING #-} MonadIO m => FrontEndIO Console m where
    
    getUpdates = pure <$> liftIO T.getLine

    sendResponse = liftIO . T.putStrLn . ("λ:" <>)

    sendWebResponse _ = pure ()