{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns #-}

module Extended.HTTP 
    ( module Network.HTTP.Types.URI
    , module Network.HTTP.Simple
    , module Network.HTTP.Client.Conduit 
    , percentEncode
    , stringEncode
    , MonadHttp(..)
    , Url
    ) where

import Control.Monad.IO.Class(MonadIO (liftIO))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Functor
import Data.Aeson
import Extended.Text (Text)
import Extended.Text qualified as T
import Network.HTTP.Types.URI ( urlEncode )
import Network.HTTP.Client.Conduit (HttpException(..), HttpExceptionContent(..))
import Network.HTTP.Simple

type Url = String

stringEncode :: T.Text -> T.Text
stringEncode = T.pack . BS8.unpack . urlEncode True . T.encodeUtf8

percentEncode :: ToJSON a => a -> T.Text
percentEncode = T.pack . BS8.unpack . urlEncode True . BSL.toStrict . encode

class MonadHttp m where
    tryRequest :: Text -> m BSL.ByteString

instance {-# OVERLAPPING #-} MonadIO m => MonadHttp m where
    tryRequest (T.unpack -> initReq) = liftIO $ do
            req <- parseRequest initReq
            -- print req
            resp <- httpBS req <&> BSL.fromStrict . getResponseBody
            -- print resp
            pure resp


