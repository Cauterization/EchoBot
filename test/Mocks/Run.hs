module Mocks.Run where

import Bot.Bot (recieveActions)
import Bot.Error (BotError (..))
import Bot.IO (FrontEndIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalState, runState)
import Control.Monad.Writer (runWriterT)
import Data.Map qualified as M
import Extended.Text qualified as T
import Logger ((>.))
import Mocks.TestBot (BotState (..), TestBot (..))
import Mocks.TestFront (TestFront (..))
import Wait (MonadWait)

runTBot ::
  forall f a.
  (TestFront f, FrontEndIO f (TestBot f), MonadWait (TestBot f)) =>
  (BotState f -> BotState f) ->
  TestBot f a ->
  IO (Either BotError a, BotState f)
runTBot f b = do
  either
    (pure . const ())
    (mapM_ (T.putStrLn . (\(v, t) -> v >. ": " <> t)))
    $ flip evalState (f initialState) $
      runExceptT $
        fmap snd $
          runWriterT $
            unwrapTB $
              recieveActions @f @(TestBot f)

  pure $
    flip runState (f initialState) $
      runExceptT $ fmap fst $ runWriterT $ unwrapTB b

execTBot ::
  forall f a.
  (TestFront f, FrontEndIO f (TestBot f), MonadWait (TestBot f)) =>
  (BotState f -> BotState f) ->
  TestBot f a ->
  IO (BotState f)
execTBot f b = snd <$> runTBot f b

evalTBot ::
  forall f a.
  (TestFront f, FrontEndIO f (TestBot f), MonadWait (TestBot f)) =>
  (BotState f -> BotState f) ->
  TestBot f a ->
  IO (Either BotError a)
evalTBot f b = fst <$> runTBot f b

initialState :: forall f. TestFront f => BotState f
initialState =
  BotState
    { bUpdates = [],
      bSenededResponse = [],
      bRepeats = M.empty,
      bDefaultRepeats = 1,
      bFrontData = initialFrontEnv @f,
      bHelpMessage = "HelpMessage",
      bRepeatMessage = "RepeatMessage",
      bIsLogerRequired = True
    }
