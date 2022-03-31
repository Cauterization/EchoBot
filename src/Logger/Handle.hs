module Logger.Handle 
    ( debug
    , info
    , warning
    , error
    , Handle(..)
    , Config(..)
    , Logger
    , Verbosity(..)
    , Mode(..)
    , HasLogger(..)
    ) where

import Control.Applicative ( Alternative((<|>)) )

import Data.Aeson hiding (Error)

import qualified Data.Text as T

import GHC.Generics ( Generic )

import Prelude hiding (error, log)

import qualified System.IO as Sys

data Verbosity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show, Generic)
instance FromJSON Verbosity

data Mode
    = None
    | Display
    | Write
    | Both
    deriving (Eq, Ord, Show, Generic)
instance FromJSON Mode

data Config = Config
    { cVerbosity :: Verbosity
    , cMode      :: Mode
    , cFilePath  :: FilePath
    } deriving (Show, Generic)
    
instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        cVerbosity <- o .: "Verbosity"
        cMode      <- o .: "Mode"
        cFilePath  <- o .: "FilePath" <|> pure "log.txt" 
        pure Config{..}

data Handle m = Handle
  { hConfig     :: Config
  , hFileHandle :: Maybe Sys.Handle
  , hLogger     :: Verbosity -> T.Text -> m ()
  }

type Logger m = Verbosity -> T.Text -> m ()

debug, info, warning, error :: (Monad m, HasLogger m) => T.Text -> m ()
debug   = mkLog Debug
info    = mkLog Info
warning = mkLog Warning
error   = mkLog Error 


class HasLogger m where
    mkLog :: Logger m