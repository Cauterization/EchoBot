module Wait where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (..))

class MonadWait m where
  wait :: Int -> m ()
  default wait :: MonadIO m => Int -> m ()
  wait = liftIO . threadDelay . (* 1000000)
