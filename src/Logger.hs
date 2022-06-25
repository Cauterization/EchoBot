{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Logger
  ( debug,
    info,
    warning,
    error,
    Config (..),
    Logger,
    Verbosity (..),
    Mode (..),
    HasLogger (..),
    (.<),
    (>.),
    runLoggerMIO,
    fromConfig,
    LoggerMIO (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Time qualified as Time
import Dhall (FromDhall, Generic)
import Extended.Text qualified as T
import Wait
import Prelude hiding (error, log)

data Verbosity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show, Generic, FromDhall)

instance FromJSON Verbosity

data Mode
  = None
  | Display
  | Write
  | Both
  deriving (Eq, Ord, Show, Generic, FromDhall)

instance FromJSON Mode

data Config = Config
  { cVerbosity :: Verbosity,
    cMode :: Mode,
    cFilePath :: FilePath
  }
  deriving (Show, Generic)
  deriving (FromDhall)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    cVerbosity <- o .: "Verbosity"
    cMode <- o .: "Mode"
    cFilePath <- o .: "FilePath" <|> pure "log.txt"
    pure Config {..}

type Logger m = Verbosity -> T.Text -> m ()

debug, info, warning, error :: (Monad m, HasLogger m) => T.Text -> m ()
debug = mkLog Debug
info = mkLog Info
warning = mkLog Warning
error = mkLog Error

class HasLogger m where
  mkLog :: Logger m

(.<) :: (Show a) => T.Text -> a -> T.Text
text .< a = text <> T.show a

infixr 7 .<

(>.) :: (Show a) => a -> T.Text -> T.Text
(>.) = flip (.<)

infixr 7 >.

fromConfig :: Config -> Logger IO
fromConfig Config {..} v t
  | v < cVerbosity = pure ()
  | otherwise = formatted >>= toOutput
  where
    formatted = do
      utcTime <- Time.getCurrentTime
      let localTime = Time.addUTCTime (10800 :: Time.NominalDiffTime) utcTime
          asctime = Time.formatTime Time.defaultTimeLocale "%a %b %d %H:%M:%S %Y" localTime
      pure $ "\n\n" <> T.pack asctime <> " " .< v <> "\n" <> t
    toOutput out = case cMode of
      None -> pure ()
      Display -> T.putStrLn out
      Write -> T.appendFile cFilePath out
      Both -> T.putStrLn out >> T.appendFile cFilePath out

newtype LoggerMIO a = LoggerMIO {unLoggerMIO :: ReaderT Config IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadThrow, MonadCatch)

deriving anyclass instance MonadWait LoggerMIO

instance HasLogger LoggerMIO where
  mkLog v t = do
    c <- ask
    liftIO $ fromConfig c v t

runLoggerMIO :: Config -> LoggerMIO a -> IO a
runLoggerMIO c = flip runReaderT c . unLoggerMIO
