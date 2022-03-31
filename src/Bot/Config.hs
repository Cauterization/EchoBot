{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Bot.Config where 

import Control.Exception (IOException, catch, fromException)
import Control.Monad

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Typeable, typeOf, Proxy (Proxy))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Foldable 
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

getConfig :: FromJSON (Config f) => FilePath -> IO (Config f)
getConfig fp = BL.readFile fp >>= either parsingFail pure . eitherDecode
  where
    parsingFail = fail . ("Parsing config error: " <>) . show 

-- withConfig :: FilePath -> (forall f. Show (Config f) => Config f -> IO a) -> IO a
-- withConfig fp f = getConfig @'Vkontakte fp >>= f 

withConfig :: FilePath -> (forall f. Show (Config f) => Config f -> IO a) -> IO a
withConfig fp f = foldl1 g $ map ($ fp)
    [ getConfig @'Vkontakte >=> f 
    , getConfig @'Telegram  >=> f
    , getConfig @'Console   >=> f
    ]
  where
    g cur next = catch cur $ \(e :: IOException) -> 
        case ioe_description e of
            "Parsing config error: \"Error in $.FrontEnd: empty\""  -> next
            _ -> cur



                                                
                                                    
                      
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
