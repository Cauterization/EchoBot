{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.BotSpec
  ( spec,
  )
where

import Bot.Bot (executeAction, recieveActions)
import Bot.FrontEnd (Action (..), frontName)
import Bot.IO (FrontEndIO)
import Bot.Types (Repeat (..))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import FrontEnd.Console.Main (Console)
import FrontEnd.Telegram.Main (Telegram)
import FrontEnd.Vkontakte.Main (Vkontakte)
import Logger ((>.))
import Logger qualified
import Mocks.Gens (EchoUpdateGen (..), RepeatEchoUpdateGen (..))
import Mocks.Predicates (isEchoUpdate, isRepeatEchoUpdate)
import Mocks.Run (evalTBot, execTBot)
import Mocks.TestBot (BotState (..), TestBot)
import Mocks.TestFront (TestFront (..), TestUpdate (..), getUserFromEchoUpdate)
import Mocks.With (withUpdate)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Property, property, (==>))

spec :: Spec
spec = do
  specFront @Vkontakte
  specFront @Telegram
  specFront @Console

specFront :: forall f. (TestFront f, FrontEndIO f (TestBot f)) => Spec
specFront = describe (frontName @f <> " common tests:") $ do
  it "should echo any non-command input back" $
    property $ propSendsEchoBack @f

  it "actually sends echo several times according to number of repeatitions" $
    property $ propSendsEchoSeveralTimes @f

propSendsEchoBack ::
  forall f.
  ( TestFront f,
    FrontEndIO f (TestBot f)
  ) =>
  EchoUpdateGen f ->
  Property
propSendsEchoBack (unEchoUpdateGen -> update) =
  isRepeatEchoUpdate @f update || isEchoUpdate @f update ==> do
    res <-
      evalTBot @f (withUpdate update) $
        recieveActions @f @(TestBot f)
    Right url <-
      evalTBot @f
        id
        (prepareRequest update)
    case toTestUpdate @f update of
      EchoRepeatUpdateT text userID ->
        res `shouldBe` Right [SendRepeatEcho userID text url]
      EchoUpdateT text userID ->
        res `shouldBe` Right [SendEcho userID text url]

propSendsEchoSeveralTimes :: forall f. (TestFront f) => RepeatEchoUpdateGen f -> Property
propSendsEchoSeveralTimes (unRepeatEchoUpdateGen -> update) =
  isRepeatEchoUpdate @f update ==> do
    s <-
      execTBot @f (withUpdate update) $
        recieveActions @f @(TestBot f) >>= mapM_ executeAction
    length (bSenededResponse s)
      `shouldBe` unRepeat
        ( fromMaybe
            (bDefaultRepeats s)
            (M.lookup (getUserFromEchoUpdate @f update) $ bRepeats s)
        )
