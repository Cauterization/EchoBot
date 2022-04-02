{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.App where

import Control.Exception (IOException, fromException)
import Control.Monad.Catch
import Control.Monad.Reader

import Data.IORef

import GHC.IO.Exception

import App.Config
import App.Env

import Bot.Action
import Bot.Bot
import Bot.Types

import Console.FrontEnd

import FrontEnd.FrontEnd
import FrontEnd.Web

import Vkontakte.Web qualified as VK

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
                     )

instance Ord (User f) => HasRepeats (App f) f where
    getRepeats u = asks (envRepeats @f) >>= (liftIO . readIORef) <&> M.lookup u


instance ( IsWebFrontEnd f (App f)
         ) => HasWebEnv f (App f) where
    getToken = asks $ envToken @f
    getFrontData = asks (envFrontData @f) >>= liftIO . readIORef
    setFrontData fd = asks (envFrontData @f) >>= liftIO . flip writeIORef fd
    getPollingTime = asks (envPollingTime @f)

instance Logger.HasLogger (App f) where
    mkLog v t = do
        l <- asks envLogger 
        liftIO $  l v t

chooseFront :: FilePath -> IO ()
chooseFront fp = foldl1 handler 
    [ getConfig @VK.Vkontakte fp >>= newEnv >>= runReaderT (unApp bot)
    -- , getConfig @'Telegram  fp >>= newEnv >>= runReaderT (unApp app)
    -- , getConfig @Console   fp >>= newEnv >>= runReaderT (unApp app)
    ]
  where
    handler cur next = catch cur $ \e -> 
        if ioe_description e == confErr <> "\"Error in $.FrontEnd: empty\""
        then next else cur
