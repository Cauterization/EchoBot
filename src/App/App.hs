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
import Bot.Types

import Console.FrontEnd

import FrontEnd.FrontEnd
import FrontEnd.Web

import Vkontakte.Web qualified as VK

import Extended.HTTP qualified as HTTP
import  Logger.Handle ((.<))
import  Logger.Handle qualified as Logger

newtype App (f :: FrontEnd) a = App {unApp :: (ReaderT (Env f) IO) a}
    deriving newtype ( Functor
                     , Applicative
                     , Monad
                     , MonadReader (Env f)
                     , MonadIO
                     , MonadThrow
                     )

instance ( IsWebFrontEnd f
         ) => HasWebEnv f (App f) where
    getToken = asks envToken
    getFrontData = asks envFrontData >>= liftIO . readIORef
    setFrontData fd = asks envFrontData >>= liftIO . flip writeIORef fd
    getPollingTime = asks envPollingTime

instance Logger.HasLogger (App f) where
    mkLog v t = do
        l <- asks envLogger 
        liftIO $  l v t

chooseFront :: FilePath -> IO ()
chooseFront fp = foldl1 handler 
    [ getConfig @'Vkontakte fp >>= newEnv >>= runReaderT (unApp app)
    -- , getConfig @'Telegram  fp >>= newEnv >>= runReaderT (unApp app)
    , getConfig @'Console   fp >>= newEnv >>= runReaderT (unApp app)
    ]
  where
    handler cur next = catch cur $ \e -> 
        if ioe_description e == confErr <> "\"Error in $.FrontEnd: empty\""
        then cur else next

app :: forall f. (IsFrontEnd f, FrontEndIO f (App f)) => App f ()
app = do
    Logger.info "Getting updates.."
    updates <- getUpdates 
    Logger.info $ "Recieved " .< length updates <> " new updates."
    forM_ (map getAction updates) $ \case
        SendEcho user se -> undefined
        -- SendHelp sh -> undefined
        -- UpdateRepeats u r -> undefined
        -- SendKeyboard sk -> undefined
        -- HideKeyboard hk -> undefined


-- import Control.Concurrent
-- import Control.Monad.Catch

-- import Control.Monad.Reader
-- import qualified Data.ByteString.Lazy as BSL
-- import Data.IORef
-- import Data.Typeable
-- import qualified Data.Map as M
-- import Data.Maybe
-- import Data.Foldable
-- import Prelude hiding (read)
-- import Extended.Aeson
-- import Extended.Text(Text)
-- import qualified Extended.HTTP as HTTP
-- import qualified Extended.Text as T
-- import app.Config
-- import app.app
-- import qualified Logger.Handle as Logger

-- import FrontEnd.FrontEnd (FrontEnd)
-- import FrontEnd.FrontEnd qualified as Front

-- data Env f = Env
--     { envLogger         :: Logger.Logger IO
--     , envToken          :: !Token
--     , envConnData       :: IORef (Front.ConnectionData f)
--     , envDefaultReplies :: !Int    
--     , envHelpMes        :: !Text
--     , envRepMes         :: !Text
--     , envReps           :: IORef (M.Map (Front.User f) Rep)
--     , envPollingTime    :: !Int
--     }



-- instance HasToken (App f) where
--     getToken = asks envToken

-- instance FrontEnd f => HasConnectionData f (App f) where
--     FrontData = asks envConnData >>= liftIO . readIORef
--     setConnData cd = asks envConnData >>= liftIO . flip writeIORef cd

-- instance FrontEnd f => HasReps f (App f) where
--     setRep u rep = do
--         ref <- asks envReps
--         reps <- liftIO $ readIORef ref
--         liftIO $ writeIORef ref $ M.insert u rep reps
--     getRep u = do
--         reps <- asks (readIORef . envReps) 
--         def  <- defaultRep @f
--         liftIO $ fromMaybe def . M.lookup u <$> reps
--     defaultRep = asks envDefaultReplies

-- instance HasMessages (App f) where
--     getHelpMes = asks envHelpMes
--     getRepMes  = asks envRepMes

-- instance HasPollingTime (App f) where
--     getPollingTime = asks envPollingTime

-- instance MonadWait (App f) where
--     wait s = liftIO $ threadDelay $ s * 1000000

-- instance Logger.HasLogger (App f) where
--     mkLog v t = do
--         l <- asks envLogger
--         liftIO $ l v t

-- runApp :: forall f . FrontEnd f => Config -> Logger.Handle IO -> IO ()
-- runApp Config{..} lh = do
--     reps <- newIORef M.empty 
--     cd <- Front.newConnectionData @f cToken >>= newIORef 
--     let env = Env 
--             { envLogger         = Logger.hLogger lh
--             , envToken          = cToken
--             , envConnData       = cd 
--             , envDefaultReplies = cDefaultReplies    
--             , envHelpMes        = cHelpMessage
--             , envRepMes         = cRepeatMessage
--             , envReps           = reps
--             , envPollingTime    = cPollingTime 
--             } :: Env f
--     runReaderT (runapp @f) env

-- data Stopapp = Stopapp deriving (Show, Typeable, Exception)

-- runapp :: forall f a. (FrontEnd f, MonadCatch a) => a ()
-- runapp = handle (\Stopapp -> pure ()) $ forever $ 
--     app @f

-- app :: forall a b. b ()
-- app = undefined

-- -- app :: forall b a . (MonadThrow a, MonadCatch a) => a ()
-- -- app = flip catches (handler @b) $ do       
-- --     Logger.info "Getting updates..."
-- --     response <- updateURL @b >>= HTTP.tryRequest
-- --     Logger.debug $ "Recieved JSON: " <> T.decodeUtf8 (BSL.toStrict response)
-- --     appResponse <- either (throwM . ParsingError . T.pack) pure $ eitherDecode response 
-- --     extractConnData @b appResponse >>= updateConnData
-- --     updates <- withBadResponseHandling appResponse >>= parseUpdates
-- --     Logger.debug $ "Parsed as:" <> T.show updates
-- --     if null updates
-- --     then Logger.info "No new updates."
-- --     else do 
-- --         Logger.info $ "Recieved " <> T.show (length updates) <> " new messages."
-- --         traverse_ processUpdate updates 
-- --         Logger.info "All echo have been sent. Going to next iteration."

-- -- processUpdate :: forall b a .
-- --     ( FrontEnd b
-- --     , Monad a
-- --     , Logger.HasLogger a
-- --     , HTTP.MonadHttp a
-- --     , MonadThrow a
-- --     , HasReps b a
-- --     , HasToken a
-- --     , HasMessages a
-- --     ) => Front.Update b -> a ()
-- -- processUpdate = \case
-- --     Front.TrashUpdate t -> 
-- --         Logger.info $ "This update doesn't looks like something meaningful:\n" <> T.show t 
-- --     Front.SetRepeatsUpdate user rep hidekeyboard -> do
-- --         Logger.debug $ "New repeats for user " <> T.show user <> " is " <> T.show rep <> "."
-- --         setRep @b user rep
-- --         processUpdate $ Front.EchoUpdate hidekeyboard
-- --     Front.EchoUpdate u -> do
-- --         (rep,req) <- prepareEcho u
-- --         Logger.debug $ "Sending req: " <> T.show req 
-- --         replicateM rep (HTTP.tryRequest req) >>= mapM_ (checkCallback @b) . listToMaybe

-- -- restartIn :: forall b a. MonadCatch a => Int -> a ()
-- -- restartIn s = do
-- --     when (s /= 0) $ Logger.info $ "Restart in " <> T.show s <> "s."
-- --     wait s 
-- --     Logger.info "Restarting." 
-- --     runapp @b

-- -- handler :: forall b a. MonadCatch a => [Handler a ()]
-- -- handler = [Handler handlerHTTP, Handler handlerapp, Handler handlerMessanger]
-- --   where

-- --     handlerHTTP e = (>> restartIn @b 60) $ Logger.error $ case e of
-- --         HTTP.HttpExceptionRequest req content 
-- --             -> "HTTP exception accured with request:\n" <> T.show req 
-- --             <> "\ncontent:\n" <> T.show content
-- --         HTTP.InvalidUrlException req content
-- --             -> "HTTP exception accured with request:\n" <> T.pack req 
-- --             <> "\ncontent:\n" <> T.pack content 

-- --     handlerapp = \case
-- --         (err :: Front.Error) -> Logger.error (T.show err) >> restartIn @b @a 60

-- --     handlerMessanger = errorHandler @b

-- -- parse :: (FromJSON x, MonadThrow a) => BSL.ByteString -> a x
-- -- parse = either (throwM . ParsingError . T.pack) pure . eitherDecode 

-- -- read  :: MonadThrow m => Text -> m Rep
-- -- read t = case T.readEither @Int $ T.unpack t of 
-- --     Right r  -> pure r 
-- --     Left err -> throwM $ ParsingError $ T.pack err