{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module App.App where

import App.Config ( confErr, getConfig )
import App.Env
    ( newEnv,
      Env(envLogger, envRepeats, envDefaultRepeats, envToken,
          envFrontData, envPollingTime, envHelpMessage, envRepeatMessage) )
import Bot.Bot ( bot )
import Bot.FrontEnd ( HasEnv(..), IsFrontEnd(BotUser) )
import Bot.IO (FrontEndIO (..))
import Console.FrontEnd (Console)
import Control.Monad.Catch ( MonadThrow, MonadCatch(..), handle )
import Control.Monad.Reader
    ( asks, (>=>), MonadReader, ReaderT(..), MonadIO(..) )
import Data.Functor ( (<&>) )
import Data.IORef ( writeIORef, readIORef )
import Data.Map qualified as M
import Extended.Text qualified as T
import GHC.IO.Exception ( IOException(ioe_description) )
import Logger.Handle qualified as Logger
import System.Exit qualified as Sys
import Telegram.FrontEnd (Telegram)
import Vkontakte.FrontEnd (Vkontakte)

run :: IO ()
run =
  handle (\(e :: IOException) -> Sys.die $ show e <> ". Terminating...") $
    chooseFront "config.json"

chooseFront :: FilePath -> IO ()
chooseFront fp =
  let runBot = newEnv >=> runReaderT (unApp bot)
   in foldl1
        handler
        [ getConfig @Vkontakte fp >>= runBot,
          getConfig @Telegram fp >>= runBot,
          getConfig @Console fp >>= runBot
        ]
  where
    handler cur next = catch cur $ \e ->
      if ioe_description e == confErr <> "\"Error in $.FrontEnd: empty\""
        then next
        else cur

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

instance FrontEndIO Console (App Console) where
  getUpdates = pure <$> liftIO T.getLine

  sendResponse = liftIO . T.putStrLn . ("λ:" <>)

  sendWebResponse _ = pure ()

instance Ord (BotUser f) => HasEnv f (App f) where
  getRepeats user = asks envRepeats >>= (liftIO . readIORef) <&> M.lookup user
  setRepeats user rep = do
    ref <- asks envRepeats
    liftIO $ readIORef ref >>= writeIORef ref . M.insert user rep
  defaultRepeats = asks envDefaultRepeats
  getToken = asks envToken
  getFrontData = asks envFrontData >>= liftIO . readIORef
  setFrontData fd = asks envFrontData >>= liftIO . flip writeIORef fd
  getPollingTime = asks envPollingTime
  getHelpMessage = asks envHelpMessage
  getRepeatMessage = asks envRepeatMessage

instance Logger.HasLogger (App f) where
  mkLog verbosity text = do
    l <- asks envLogger
    liftIO $ l verbosity text
