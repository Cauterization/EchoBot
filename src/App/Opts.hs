module App.Opts where

import Options.Applicative

newtype Options = Options
  { optConfigPath :: FilePath
  }

configFileParser :: Parser FilePath
configFileParser =
  strOption
    ( long "conf"
        <> help "Configuration file location"
    )

optionsParser :: Parser Options
optionsParser = Options <$> configFileParser

runWithOpts :: IO Options
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc
