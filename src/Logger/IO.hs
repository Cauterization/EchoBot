module Logger.IO
  ( fromConfig,
  )
where

import Data.Time
    ( defaultTimeLocale,
      formatTime,
      addUTCTime,
      getCurrentTime,
      NominalDiffTime )
import Extended.Text qualified as T
import Logger.Handle ( Logger, Config(..), Mode(..), (.<) )

fromConfig :: Config -> Logger IO
fromConfig Config {..} v t
  | v < cVerbosity = pure ()
  | otherwise = formatted >>= toOutput
  where
    formatted = do
      utcTime <- getCurrentTime
      let localTime = addUTCTime (10800 :: NominalDiffTime) utcTime
          asctime = formatTime defaultTimeLocale "%a %b %d %H:%M:%S %Y" localTime
      pure $ "\n\n" <> T.pack asctime <> " " .< v <> "\n" <> t
    toOutput out = case cMode of
      None -> pure ()
      Display -> T.putStrLn out
      Write -> T.appendFile cFilePath out
      Both -> T.putStrLn out >> T.appendFile cFilePath out