module Logger.IO 
    ( fromConfig
    ) where

import Data.Time 
import Extended.Text qualified as T
import Logger.Handle

fromConfig :: Config -> Logger IO
fromConfig Config{..} v t
    | v < cVerbosity = pure ()
    | otherwise = formatted >>= T.putStrLn
  where
    formatted = do 
        utcTime <- getCurrentTime
        let localTime = addUTCTime (10800 :: NominalDiffTime) utcTime
            asctime = formatTime defaultTimeLocale "%a %b %d %H:%M:%S %Y" localTime
        pure $ "\n\n" <> T.pack asctime <> " " .< v <> "\n" <> t
