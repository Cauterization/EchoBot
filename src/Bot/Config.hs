{-# LANGUAGE  DeriveAnyClass #-}
{-# LANGUAGE  EmptyDataDeriving #-}
{-# LANGUAGE  StandaloneDeriving #-}
{-# LANGUAGE  ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Bot.Config where 
    
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
import Types

import qualified Extended.Text as T

type Repet = Int

type Token = Text

data FrontEnd = Vkontakte | Telegram | Console deriving (Show, Generic, FromJSON)

data NotRequired = NotRequired deriving Show

instance FromJSON NotRequired where
  parseJSON _ = pure NotRequired

newtype FrontName (f :: FrontEnd) = FrontName FrontEnd
    deriving (Generic)
    deriving newtype (Show)

instance Typeable f => FromJSON (FrontName f) where
    parseJSON = withText "FrontEnd" $ \t -> do
        guard $ "Proxy FrontEnd '" <> t == T.show (typeOf (Proxy @f))
        FrontName <$> parseJSON (String t)

type family WebField (f :: FrontEnd) a where
    WebField 'Console a = NotRequired
    WebField f        a = a

data Config (f :: FrontEnd) = Config
    { cLogger         :: Logger.Config
    , cHelpMessage    :: !Text
    , cRepeatMessage  :: !Text
    , cFrontEnd       :: FrontName f
    , cToken          :: WebField  f Token
    , cPollingTime    :: WebField  f Int
    } deriving stock (Generic)

deriving via (CustomJSON '[FieldLabelModifier (StripPrefix "c")] (Config f)) instance 
    (  Typeable f
    ,  FromJSON (WebField f Token)
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

withConfig :: FilePath -> (forall f. Show (Config f) => Config f -> IO a) -> IO a
withConfig fp f = asum $ map ($ fp)
    [ getConfig @'Vkontakte >=> f 
    , getConfig @'Telegram  >=> f
    , getConfig @'Console   >=> f
    ]
                                 
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
