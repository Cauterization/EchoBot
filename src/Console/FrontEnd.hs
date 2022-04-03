{-# LANGUAGE MultiWayIf #-}
module Console.FrontEnd where

import Data.Text qualified as T

import Extended.Text (Text)
import Extended.Text qualified as T

import Bot.Error
import Bot.FrontEnd
import Bot.Types
import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics

import Bot.IO
import Control.Applicative
import Data.Function (on)
import qualified Data.ByteString.Lazy as BSL
import Data.String
import Control.Monad.Catch

data Console = Console deriving (Generic, FromJSON, Show)

instance IsFrontEnd Console where

    type WebOnly   Console _ = NotRequired

    type User      Console = NotRequired

    type Update    Console = Text

    -- | Is bot waits to number of repeatitions ?
    type FrontData Console = Bool

    newFrontData _ = pure False

    getActions = getAction

instance {-# OVERLAPPING #-} MonadIO m => FrontEndIO Console m where
    
    getUpdates = pure <$> liftIO T.getLine

    sendResponse = liftIO . T.putStrLn

    sendWebResponse _ = pure ()

getAction :: (Monad m, HasEnv Console m, MonadThrow m) => Update Console -> m [Action Console]
getAction = \case
    "/help"   -> pure . SendEcho <$> getHelpMessage
    "/repeat" -> setFrontData True >> liftA2 
        ((<>) `on` pure) (SendEcho <$> getRepeatMessage) (pure $ SendKeyboard NotRequired)
    t         -> do
        fd <- getFrontData
        if fd 
        then setFrontData False >> pure . UpdateRepeats NotRequired <$> parse @Repeat (fromString $ T.unpack t)
        else pure . pure $ SendRepeatEcho NotRequired t