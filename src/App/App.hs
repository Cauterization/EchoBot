{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.App where

import Control.Exception (IOException, fromException)
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Map             qualified as M
import Data.IORef
import Data.Functor
import GHC.IO.Exception
import System.Exit          qualified as Sys

import App.Config
import App.Env
import Bot.Bot
import Bot.FrontEnd
import Bot.Types
import Console.FrontEnd   (Console)
import Extended.HTTP        qualified as HTTP
import Logger.Handle ((.<))
import Logger.Handle qualified as Logger
import Telegram.FrontEnd  (Telegram)
import Vkontakte.FrontEnd (Vkontakte)

run :: IO ()
run = handle (\(e :: IOException) -> Sys.die $ show e <> ". Terminating...") 
    $ chooseFront "config.json"

chooseFront :: FilePath -> IO ()
chooseFront fp = foldl1 handler 
    [ getConfig @Vkontakte fp >>= newEnv >>= runReaderT (unApp bot)
    , getConfig @Telegram  fp >>= newEnv >>= runReaderT (unApp bot)
    , getConfig @Console   fp >>= newEnv >>= runReaderT (unApp bot)
    ]
  where
    handler cur next = catch cur $ \e -> 
        if ioe_description e == confErr <> "\"Error in $.FrontEnd: empty\""
        then next else cur

newtype App f a = App {unApp :: (ReaderT (Env f) IO) a}
    deriving newtype 
        ( Functor
        , Applicative
        , Monad
        , MonadReader (Env f)
        , MonadIO
        , MonadThrow
        , MonadCatch
        )

instance Ord (BotUser f) => HasEnv f (App f) where
    getRepeats user  = asks envRepeats >>= (liftIO . readIORef) <&> M.lookup user
    setRepeats user rep = do
        ref <- asks envRepeats
        liftIO $ readIORef ref >>=  writeIORef ref . M.insert user rep
    defaultRepeats   = asks envDefaultRepeats
    getToken         = asks envToken 
    getFrontData     = asks envFrontData >>= liftIO . readIORef
    setFrontData fd  = asks envFrontData >>= liftIO . flip writeIORef fd
    getPollingTime   = asks envPollingTime
    getHelpMessage   = asks envHelpMessage 
    getRepeatMessage = asks envRepeatMessage  

instance Logger.HasLogger (App f) where
    mkLog verbosity text = do
        l <- asks envLogger 
        liftIO $  l verbosity text



