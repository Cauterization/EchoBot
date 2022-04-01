{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Config where 


import Control.Monad ( (>=>), MonadPlus (mzero) )

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Typeable, typeOf, Proxy (Proxy))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text) 

import Deriving.Aeson (CustomJSON(..), FieldLabelModifier, StripPrefix)

import GHC.Generics (Generic)

import Logger.Handle qualified as Logger

import Bot.Types 

import FrontEnd.FrontEnd 

import qualified Extended.Text as T

data Config (f :: FrontEnd) = Config
    { cLogger         :: Logger.Config
    , cDefaultRepeats :: Repeat
    , cHelpMessage    :: !Text
    , cRepeatMessage  :: !Text
    , cFrontEnd       :: FrontName f
    , cToken          :: WebOnly   f (Token f)
    , cPollingTime    :: WebOnly   f PollingTime
    } deriving stock (Generic)

deriving via (CustomJSON '[FieldLabelModifier (StripPrefix "c")] (Config f)) instance 
    (  Typeable f
    ,  FromJSON (WebOnly f (Token f))
    ,  FromJSON (WebOnly f Int)) 
    => FromJSON (Config  f)

-- deriving instance Show (Config 'Vkontakte)
-- deriving instance Show (Config 'Telegram)
deriving instance Show (Config 'Console)

confErr :: String
confErr = "Parsing config error: "

getConfig :: FromJSON (Config f) => FilePath -> IO (Config f)
getConfig fp = BL.readFile fp >>= either parsingFail pure . eitherDecode
  where
    parsingFail = fail . (confErr <>) . show 




                                                
                                                    
                      
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
