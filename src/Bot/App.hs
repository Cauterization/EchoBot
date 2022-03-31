module Bot.App where

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
-- import Bot.Config
-- import Bot.Bot
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

-- newtype App f a = App {unApp :: (ReaderT (Env f) IO) a}
--     deriving newtype ( Functor
--                      , Applicative
--                      , Monad
--                      , MonadReader (Env f)
--                      , MonadIO
--                      , HTTP.MonadHttp 
--                      )

-- instance HasToken (App f) where
--     getToken = asks envToken

-- instance FrontEnd f => HasConnectionData f (App f) where
--     getConnData = asks envConnData >>= liftIO . readIORef
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
--     runReaderT (runBot @f) env

-- data StopBot = StopBot deriving (Show, Typeable, Exception)

-- runBot :: forall f a. (FrontEnd f, MonadCatch a) => a ()
-- runBot = handle (\StopBot -> pure ()) $ forever $ 
--     bot @f

-- bot :: forall a b. b ()
-- bot = undefined

-- -- bot :: forall b a . (MonadThrow a, MonadCatch a) => a ()
-- -- bot = flip catches (handler @b) $ do       
-- --     Logger.info "Getting updates..."
-- --     response <- updateURL @b >>= HTTP.tryRequest
-- --     Logger.debug $ "Recieved JSON: " <> T.decodeUtf8 (BSL.toStrict response)
-- --     botResponse <- either (throwM . ParsingError . T.pack) pure $ eitherDecode response 
-- --     extractConnData @b botResponse >>= updateConnData
-- --     updates <- withBadResponseHandling botResponse >>= parseUpdates
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
-- --     runBot @b

-- -- handler :: forall b a. MonadCatch a => [Handler a ()]
-- -- handler = [Handler handlerHTTP, Handler handlerBot, Handler handlerMessanger]
-- --   where

-- --     handlerHTTP e = (>> restartIn @b 60) $ Logger.error $ case e of
-- --         HTTP.HttpExceptionRequest req content 
-- --             -> "HTTP exception accured with request:\n" <> T.show req 
-- --             <> "\ncontent:\n" <> T.show content
-- --         HTTP.InvalidUrlException req content
-- --             -> "HTTP exception accured with request:\n" <> T.pack req 
-- --             <> "\ncontent:\n" <> T.pack content 

-- --     handlerBot = \case
-- --         (err :: Front.Error) -> Logger.error (T.show err) >> restartIn @b @a 60

-- --     handlerMessanger = errorHandler @b

-- -- parse :: (FromJSON x, MonadThrow a) => BSL.ByteString -> a x
-- -- parse = either (throwM . ParsingError . T.pack) pure . eitherDecode 

-- -- read  :: MonadThrow m => Text -> m Rep
-- -- read t = case T.readEither @Int $ T.unpack t of 
-- --     Right r  -> pure r 
-- --     Left err -> throwM $ ParsingError $ T.pack err