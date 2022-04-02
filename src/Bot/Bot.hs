{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.Bot where

import Control.Exception (IOException, fromException)
import Control.Monad.Catch
import Control.Monad.Reader

import Data.IORef

import GHC.IO.Exception

import App.Config
import App.Env

import Bot.Action
import Bot.Types

import Console.FrontEnd

import FrontEnd.FrontEnd
import FrontEnd.Web

import Vkontakte.Web qualified as VK
import Vkontakte.Web qualified as VK

import Extended.HTTP qualified as HTTP
import  Logger.Handle ((.<))
import  Logger.Handle qualified as Logger
import qualified Data.Map as M
import Data.Functor
import Control.Applicative

bot :: forall f m. 
    ( Monad m
    , MonadCatch m
    , Logger.HasLogger m
    , HTTP.MonadHttp m
    , IsFrontEnd f
    , FrontEndIO f m
    , HasEnv f m
    , MonadIO m
    ) => m ()
bot = do
    Logger.info "Getting updates.."
    updates <- getUpdates @f
    Logger.info $ "Recieved " .< length updates <> " new updates."
    actions <- concat <$> mapM getActions updates
    forM_ actions $ \case
        SendEcho user se -> replicate . unRepeat <$> getRepeatsFor user <*> pure se
        -- SendHelp sh -> 
        -- UpdateRepeats u r -> 
        -- SendKeyboard sk -> 
        -- HideKeyboard hk -> 
