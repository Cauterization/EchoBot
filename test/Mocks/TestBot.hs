{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Mocks.TestBot where

import Bot.Error (BotError)
import Bot.FrontEnd (IsFrontEnd (..))
import Bot.Types (Repeat, URL)
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..), SomeException (..), fromException)
import Control.Monad.Except (ExceptT (..), MonadError, throwError)
import Control.Monad.Extra (when, whenM)
import Control.Monad.State (MonadState, State, gets)
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.Map qualified as M
import Extended.Text (Text)
import Logger qualified
import Wait (MonadWait (..))

newtype TestBot f a = TestBot
  { unwrapTB ::
      WriterT
        [(Logger.Verbosity, Text)]
        ( ExceptT
            BotError
            (State (BotState f))
        )
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadWriter [(Logger.Verbosity, Text)],
      MonadError BotError,
      MonadState (BotState f)
    )

instance MonadThrow (TestBot f) where
  throwM e = case fromException (SomeException e) of
    Just err -> throwError err
    _ -> error "non-bot exception."

instance MonadCatch (TestBot f) where
  catch = undefined

instance Logger.HasLogger (TestBot f) where
  mkLog v t =
    whenM (gets bIsLogerRequired) $
      when (v >= Logger.Warning) $
        tell [(v, t)]

instance MonadWait (TestBot f) where
  wait _ = pure ()

data BotState f = BotState
  { bUpdates :: [Update f],
    bSenededResponse :: [URL],
    bRepeats :: M.Map (BotUser f) Repeat,
    bDefaultRepeats :: Repeat,
    bFrontData :: BotFrontEnv f,
    bHelpMessage :: Text,
    bRepeatMessage :: Text,
    bIsLogerRequired :: Bool
  }
