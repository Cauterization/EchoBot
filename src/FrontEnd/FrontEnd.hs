{-# LANGUAGE GADTs  #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE FunctionalDependencies #-}

module FrontEnd.FrontEnd where
    
import Control.Arrow

import Control.Monad (guard, (>=>), liftM2)
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Aeson 
import Data.Kind (Type, Constraint)
import Data.List qualified as L
import Data.Maybe
import Data.String
import Data.Typeable (Typeable, typeOf, Proxy (..))

import Extended.Text (Text)
import Extended.Text qualified as T
import Extended.HTTP qualified as HTTP

import GHC.Generics (Generic (Rep))

import Bot.Error
import Bot.Types

import Deriving.Aeson
import Logger.Handle ((>.))
import qualified Logger.Handle as Logger
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Data.Functor

data FrontEnd 
    = Vkontakte 
    -- | Telegram
    | Console 
    deriving (Show, Generic, FromJSON)

frontName :: forall (f :: FrontEnd) s. (Typeable f, IsString s) => s
frontName = 
    let fullName = show (typeOf (Proxy @f))
    in fromString $ fromMaybe fullName $ L.stripPrefix "Proxy FrontEnd '" fullName

newtype Token (f :: FrontEnd) = Token Text 
    deriving (Show)

instance Typeable f => FromJSON (Token f) where
    parseJSON = withObject "Token" $ \v -> Token <$> v .: (frontName @f)

type family WebOnly (f :: FrontEnd) a where
    WebOnly 'Console a = NotRequired
    WebOnly f        a = a

data NotRequired = NotRequired deriving Show

instance FromJSON NotRequired where
  parseJSON _ = pure NotRequired

newtype FrontName (f :: FrontEnd) = FrontName FrontEnd
    deriving (Generic, Show)

instance Typeable f => FromJSON (FrontName f) where
    parseJSON = withText "FrontEnd" $ \t -> do
        guard $ "Proxy FrontEnd '" <> t == T.show (typeOf (Proxy @f))
        FrontName <$> parseJSON (String t)

class IsFrontEnd (f :: FrontEnd) where 

    type User f :: Type

    type FrontData f :: Type 

    newFrontData :: (Monad m, MonadThrow m, MonadIO m) 
         => WebOnly f (Token f) -> m (FrontData f)

    -- type SendEcho      f :: Type
    -- type SendHelp      f :: Type
    -- type SendKeyboard  f :: Type
    -- type HideKeyboard  f :: Type

    type Update f :: Type

    getActions :: Update f -> [Action f]

data Action (f :: FrontEnd) =
    SendEcho (User f) URL
    -- | SendHelp (SendHelp f)
    -- | UpdateRepeats (User f) Repeat
    -- | SendKeyboard (WebOnly f (SendKeyboard f))
    -- | HideKeyboard (HideKeyboard f)

class FrontEndIO (f :: FrontEnd) (m :: Type -> Type) | m -> f where

    getUpdates :: m [Update f]

    sendResponse :: Text -> m ()


instance {-# OVERLAPPABLE #-}
    ( IsFrontEnd f
    , IsWebFrontEnd f
    , Monad m
    , MonadThrow m
    , HTTP.MonadHttp m
    , Logger.HasLogger m
    , MonadThrow m
    , HasWebEnv f m
    , FromJSON (Response f)
    , FromJSON (BadResponse f)
    , Show (Response f)
    ) 
    => FrontEndIO (f :: FrontEnd) m where

    getUpdates = 
        getUpdatesURL @f <$> getToken <*> getFrontData <*> getPollingTime 
        >>= HTTP.tryRequest 
        >>= \x -> case eitherDecode @(Response f) x of
            Left _ -> parse @(BadResponse f) x >>= fmap (fmap (const [])) handleBadResponse
            Right r -> do
                Logger.debug $ "Recieved response:" Logger..< r
                setFrontData $ extractFrontData @f r 
                pure $ extractUpdates @f r

    sendResponse = HTTP.tryRequest >=> checkCallback @f

class ( IsWebFrontEnd f 
      ) => HasWebEnv (f :: FrontEnd) m | m -> f where
    getFrontData   :: m (FrontData f)
    setFrontData   :: FrontData f -> m ()
    getToken       :: m (Token f)
    getPollingTime :: m PollingTime

class ( WebOnly f URL ~ URL
      , WebOnly f (Token f) ~ Token f
      , WebOnly f (FrontData f) ~ FrontData f
      , WebOnly f PollingTime ~ PollingTime
      ) => IsWebFrontEnd (f :: FrontEnd) where

    type Response f :: Type

    extractFrontData :: Response f -> FrontData f

    extractUpdates :: Response f -> [Update f]
          
    getUpdatesURL :: Token f -> FrontData f -> PollingTime -> URL

    type BadResponse f :: Type

    handleBadResponse :: (Logger.HasLogger m, HTTP.MonadHttp m, MonadThrow m, HasWebEnv f m) => BadResponse f -> m ()

    -- type SendKeyboard  f :: Type
    -- type HideKeyboard  f :: Type

    checkCallback :: BL.ByteString -> m ()

-- withConnectionData 

-- class Action (f :: FrontEnd) where
--     type SendHelp f :: WebOnly f ()
--     sendKeyboard  :: WebOnly f (SendKeyboard f) -> Action f
--     spdateRepeats :: WebOnly f (UpdateRepeats f) -> Action f
--     hideKeyboard  :: WebOnly f (HideKeyboard f -> Action f)
