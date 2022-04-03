module Bot.Bot where

import Control.Monad.Catch ( MonadCatch )
import Control.Monad.Reader
import App.Env
import Bot.FrontEnd
import Bot.IO
import Extended.HTTP qualified as HTTP
import Logger.Handle ((.<))
import Logger.Handle qualified as Logger

bot :: forall f m. 
    ( Monad m
    , MonadCatch m
    , Logger.HasLogger m
    , HTTP.MonadHttp m
    , IsFrontEnd f
    , FrontEndIO f m
    , HasEnv f m
    ) => m ()
bot = forever $ {-handle handler $-} do
    Logger.info "Getting updates.."
    updates <- getUpdates @f
    Logger.info $ "Recieved " .< length updates <> " new updates."
    actions <- concat <$> mapM getActions updates
    forM_ actions $ \case
        SendEcho echo          -> sendResponse @f echo
        SendRepeatEcho user se -> getRepeatsFor user >>= flip replicateM_ (sendResponse @f se)
        UpdateRepeats u r      -> setRepeats u r 
        SendKeyboard sk        -> sendWebResponse @f sk
        HideKeyboard hk        -> sendWebResponse @f hk

-- handler = undefined