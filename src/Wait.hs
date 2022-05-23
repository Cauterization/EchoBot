module Wait where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (..))

class MonadWait m where
  wait :: Int -> m ()

instance MonadIO m => MonadWait m where
  wait = liftIO . threadDelay . (* 1000000)
