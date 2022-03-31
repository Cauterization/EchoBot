{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Bot.Config where 

import Control.Exception (IOException, catch, fromException)
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

import GHC.IO.Exception

import qualified Extended.Text as T

data Config (f :: FrontEnd) = Config
    { cLogger         :: Logger.Config
    , cDefaultRepeats :: Repeat
    , cHelpMessage    :: !Text
    , cRepeatMessage  :: !Text
    , cFrontEnd       :: FrontName f
    , cToken          :: WebField  f (Token f)
    , cPollingTime    :: WebField  f Int
    } deriving stock (Generic)

deriving via (CustomJSON '[FieldLabelModifier (StripPrefix "c")] (Config f)) instance 
    (  Typeable f
    ,  FromJSON (WebField f (Token f))
    ,  FromJSON (WebField f Int)) 
    => FromJSON (Config f)

deriving instance Show (Config 'Vkontakte)
deriving instance Show (Config 'Telegram)
deriving instance Show (Config 'Console)

class HasPollingTime m where
    getPollingTime :: m Int

confErr :: String
confErr = "Parsing config error: "

getConfig :: FromJSON (Config f) => FilePath -> IO (Config f)
getConfig fp = BL.readFile fp >>= either parsingFail pure . eitherDecode
  where
    parsingFail = fail . (confErr <>) . show 

withConfig :: FilePath -> (forall f. Show (Config f) => Config f -> IO a) -> IO a
withConfig fp f = foldl1 handler $ map ($ fp)
    [ getConfig @'Vkontakte >=> f 
    , getConfig @'Telegram  >=> f
    , getConfig @'Console   >=> f
    ]
  where
    handler cur next = catch cur $ \e -> 
        if ioe_description e == confErr <> "\"Error in $.FrontEnd: empty\""
        then next else cur



                                                
                                                    
                      
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
