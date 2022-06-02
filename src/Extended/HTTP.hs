{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}

module Extended.HTTP
  ( module Network.HTTP.Types.URI,
    module Network.HTTP.Simple,
    module Network.HTTP.Client.Conduit,
    percentEncode,
    stringEncode,
    MonadHttp (..),
    Url,
  )
where

import Bot.Types (PollingTime)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Functor ((<&>))
import Extended.Text (Text)
import Extended.Text qualified as T
import Logger ((.<))
import Logger qualified
import Network.HTTP.Client.Conduit (HttpException (..), HttpExceptionContent (..), ResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Simple
import Network.HTTP.Types.URI (urlEncode)

type Url = String

stringEncode :: T.Text -> T.Text
stringEncode = T.pack . BS8.unpack . urlEncode True . T.encodeUtf8

percentEncode :: ToJSON a => a -> T.Text
percentEncode = T.pack . BS8.unpack . urlEncode True . BSL.toStrict . encode

class MonadHttp m where
  tryRequest :: PollingTime -> Text -> m BSL.ByteString

instance (MonadIO m, Logger.HasLogger m) => MonadHttp m where
  tryRequest polling (T.unpack -> initReq) = do
    req <- liftIO $ parseRequest initReq
    Logger.debug $ "Outcomming request:\n" .< req
    let timeout = responseTimeoutMicro $ 1000000 * (polling + 5)
    response <-
      httpBS (setRequestResponseTimeout timeout req)
        <&> BSL.fromStrict . getResponseBody
    Logger.debug $ "Recieved raw response:\n" .< response
    pure response
