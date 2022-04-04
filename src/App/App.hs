{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module App.App where

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
import Bot.IO
import Console.FrontEnd   (Console)
import Logger.Handle qualified as Logger
import Telegram.FrontEnd  (Telegram)
import Vkontakte.FrontEnd (Vkontakte)
import Bot.IO (FrontEndIO(..))
import qualified Extended.Text as T

run :: IO ()
run = handle (\(e :: IOException) -> Sys.die $ show e <> ". Terminating...") 
    $ chooseFront "config.json"

chooseFront :: FilePath -> IO ()
chooseFront fp = let runBot = newEnv >=> runReaderT (unApp bot) 
    in foldl1 handler 
    [ getConfig @Vkontakte fp >>= runBot
    , getConfig @Telegram  fp >>= runBot
    , getConfig @Console   fp >>= runBot
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

instance FrontEndIO Console (App Console) where
    
    getUpdates = pure <$> liftIO T.getLine

    sendResponse = liftIO . T.putStrLn . ("λ:" <>)

    sendWebResponse _ = pure ()

instance Ord (BotUser f) => HasEnv f (App f) where
    getRepeats user  = asks envRepeats >>= (liftIO . readIORef) <&> M.lookup user
    setRepeats user rep = do
        ref <- asks envRepeats
        liftIO $ readIORef ref >>= writeIORef ref . M.insert user rep
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
        liftIO $ l verbosity text
