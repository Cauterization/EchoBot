module App.Opts where

import Control.Monad (when)
import Extended.Text (Text)
import Extended.Text qualified as T
import Options.Applicative

data Options = Options
  { optConfigPath :: FilePath,
    optShowHelpMessage :: Bool
  }

configFileParser :: Parser FilePath
configFileParser =
  strOption
    ( long "conf"
    )

helpMsgParser :: Parser Bool
helpMsgParser =
  switch
    ( long "help"
        <> short 'h'
    )

optionsParser :: Parser Options
optionsParser = Options <$> configFileParser <*> helpMsgParser

runWithOpts :: IO Options
runWithOpts = do
  Options {..} <- execParser opts
  when optShowHelpMessage $ T.putStrLn helpMessage
  pure Options {..}
  where
    opts = info (optionsParser <**> helper) fullDesc

helpMessage :: Text
helpMessage =
  "This is a simple echo-bot. Aviable commands: \
  \\n-h/--help - see this message;\
  \\n--conf FILEPATH - define configuration file location. "
