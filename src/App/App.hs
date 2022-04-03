{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.App where

import Control.Exception (IOException, fromException)
import Control.Monad.Catch
import Control.Monad.Reader

import Data.IORef

import GHC.IO.Exception

import App.Config
import App.Env

import Bot.Bot
import Bot.FrontEnd
import Bot.Types

import Console.FrontEnd
import Vkontakte.FrontEnd (Vkontakte)
import Telegram.FrontEnd (Telegram)

import Extended.HTTP qualified as HTTP
import  Logger.Handle ((.<))
import  Logger.Handle qualified as Logger
import qualified Data.Map as M
import Data.Functor

newtype App f a = App {unApp :: (ReaderT (Env f) IO) a}
    deriving newtype ( Functor
                     , Applicative
                     , Monad
                     , MonadReader (Env f)
                     , MonadIO
                     , MonadThrow
                     , MonadCatch
                     )

instance Ord (BotUser f) => HasEnv f (App f) where
    getRepeats user  = asks envRepeats>>= (liftIO . readIORef) <&> M.lookup user
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
    mkLog v t = do
        l <- asks envLogger 
        liftIO $  l v t

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
