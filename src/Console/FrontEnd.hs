module Console.FrontEnd where

import Data.Text qualified as T

import Extended.Text (Text)
import Extended.Text qualified as T

import FrontEnd.FrontEnd
import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics

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
    
    getUpdates = fmap pure $ liftIO $ T.getLine

    sendResponse = liftIO . T.putStrLn

getAction :: Text -> Action Console
getAction = \case
    -- "/help" -> 
    -- "/repeat" -> undefined
    t -> SendEcho NotRequired t