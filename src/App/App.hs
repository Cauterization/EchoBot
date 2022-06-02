{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module App.App where

import App.Config
  ( Config (..),
    ConfigFront (Console, Telegram, Vkontakte),
    getConfig,
  )
import App.Env
  ( Env
      ( envDefaultRepeats,
        envFront,
        envHelpMessage,
        envLogger,
        envRepeatMessage,
        envRepeats
      ),
    newEnv,
  )
import App.Opts
  ( Options (..),
    runWithOpts,
  )
import Bot.Bot (runBot)
import Bot.FrontEnd (HasEnv (..), IsFrontEnd)
import Bot.IO (FrontEndIO (..), IsBot)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Reader
  ( MonadIO (..),
    MonadReader,
    ReaderT (..),
    asks,
  )
import Data.Functor ((<&>))
import Data.IORef (readIORef, writeIORef)
import Data.Map qualified as M
import FrontEnd.Console.Main qualified as Console
import FrontEnd.Telegram.Main qualified as TG
import FrontEnd.Vkontakte.Main qualified as VK
import Logger qualified
import Wait (MonadWait)

run :: IO ()
run = do
  Options {..} <- runWithOpts
  Config {..} <- getConfig optConfigPath
  case cFrontEnd of
    Vkontakte -> start @VK.Vkontakte Config {..}
    Console -> start @Console.Console Config {..}
    Telegram -> start @TG.Telegram Config {..}

start :: forall f. (IsBot f (App f)) => Config -> IO ()
start Config {..} = do
  env <- Logger.runLoggerMIO cLogger $ newEnv @f Config {..}
  runReaderT (unApp $ runBot @f @(App f)) env

newtype App f a = App {unApp :: (ReaderT (Env f) IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Env f),
      MonadIO,
      MonadThrow,
      MonadCatch
    )

instance IsFrontEnd f => HasEnv f (App f) where
  getRepeats user = asks envRepeats >>= (liftIO . readIORef) <&> M.lookup user
  setRepeats user rep = do
    ref <- asks envRepeats
    liftIO $ readIORef ref >>= writeIORef ref . M.insert user rep
  defaultRepeats = asks envDefaultRepeats
  getHelpMessage = asks envHelpMessage
  getRepeatMessage = asks envRepeatMessage
  getFrontEnv = asks envFront >>= liftIO . readIORef
  setFrontEnv f = do
    ref <- asks envFront
    liftIO $ readIORef ref >>= writeIORef ref . f

instance Logger.HasLogger (App f) where
  mkLog verbosity text = do
    l <- asks envLogger
    liftIO $ l verbosity text

deriving anyclass instance MonadWait (App f)

deriving anyclass instance FrontEndIO VK.Vkontakte (App VK.Vkontakte)

deriving anyclass instance FrontEndIO TG.Telegram (App TG.Telegram)

instance FrontEndIO Console.Console (App Console.Console) where
  getUpdates = Console.getConsoleUpdates
  sendResponse = Console.sendConsoleResponse
