module App.Config where

import Bot.FrontEnd (FrontName, IsFrontEnd (WebOnly), Token)
import Bot.Types (PollingTime, Repeat)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.Data (Typeable)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, StripPrefix)
import GHC.Generics (Generic)
import Logger.Handle qualified as Logger

data Config f = Config
  { cLogger :: !Logger.Config,
    cDefaultRepeats :: !Repeat,
    cHelpMessage :: !Text,
    cRepeatMessage :: !Text,
    cFrontEnd :: !(FrontName f),
    cToken :: !(WebOnly f (Token f)),
    cPollingTime :: !(WebOnly f PollingTime)
  }
  deriving stock (Generic)

deriving via
  (CustomJSON '[FieldLabelModifier (StripPrefix "c")] (Config f))
  instance
    ( Typeable f,
      FromJSON f,
      FromJSON (WebOnly f (Token f)),
      FromJSON (WebOnly f PollingTime)
    ) =>
    FromJSON (Config f)

confErr :: String
confErr = "Parsing config error: "

getConfig :: FromJSON (Config f) => FilePath -> IO (Config f)
getConfig fp = BL.readFile fp >>= either parsingFail pure . eitherDecode
  where
    parsingFail = fail . (confErr <>) . show
