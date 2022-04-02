module Console.FrontEnd where

import Data.Text qualified as T

import Extended.Text (Text)
import Extended.Text qualified as T

import Bot.FrontEnd
import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics

import Bot.IO

data Console = Console deriving (Generic, FromJSON, Show)

instance IsFrontEnd Console where

    type WebOnly   Console _ = NotRequired

    type User      Console = NotRequired

    type Update    Console = Text

    type FrontData Console = NotRequired

    newFrontData _ = pure NotRequired

    -- type SendEcho  'Console  = NotRequired
    -- type SendHelp  'Console  = NotRequired

    getActions = pure . pure . getAction

instance {-# OVERLAPPING #-} MonadIO m => FrontEndIO Console m where
    
    getUpdates = pure <$> liftIO T.getLine

    sendResponse = liftIO . T.putStrLn

    sendWebResponse _ = pure ()

getAction :: Update Console -> Action Console
getAction = \case
    -- "/help" -> 
    -- "/repeat" -> 
    t -> SendEcho NotRequired t