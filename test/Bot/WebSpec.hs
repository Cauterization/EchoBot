{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.WebSpec
  ( spec,
  )
where

import Bot.Bot (executeAction, recieveActions)
import Bot.FrontEnd (HasEnv (..), Update, frontName)
import Bot.IO (FrontEndIO)
import Control.Monad (join)
import Data.Map qualified as M
import Extended.HTTP qualified as HTTP
import Extended.Text (Text)
import Extended.Text qualified as T
import FrontEnd.Telegram.Main (Telegram)
import FrontEnd.Vkontakte.Main (Vkontakte)
import Mocks.Predicates (isHelpUpdate, isRepeatUpdate, isUpdateRepeatsUpdate)
import Mocks.Run (evalTBot, execTBot)
import Mocks.TestBot (BotState (..), TestBot)
import Mocks.TestFront (TestFront (..), TestUpdate (..))
import Mocks.With (withHelpText, withRepeatText, withUpdate, withUpdates)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.QuickCheck (Property, property, (==>))

spec :: Spec
spec = do
  specWeb @Vkontakte
  specWeb @Telegram

specWeb :: forall f. (TestFront f, FrontEndIO f (TestBot f)) => Spec
specWeb = describe (frontName @f <> " web tests:") $ do
  it "should update counter when it recieves updateRepeats update" $
    property $ propWebUpdatesCounter @f

  it "should send corresponding messages according to recieved commands" $
    property $ propWebSendsCommands @f

  it "this messages are percent-encoded" $
    property $
      join $ \text update -> do
        HTTP.stringEncode text /= text ==> propWebSendsCommands @f text

  it "only last taken repeatition is taken into account" $
    property $ propWebSetsLastsRepeatition @f

propWebUpdatesCounter :: forall f. TestFront f => Update f -> Property
propWebUpdatesCounter update =
  isUpdateRepeatsUpdate @f update ==> do
    res <-
      execTBot @f (withUpdates [update]) $
        recieveActions @f @(TestBot f) >>= mapM_ (executeAction @f)
    case toTestUpdate @f update of
      UpdateRepeatsT user rep ->
        M.lookup user (bRepeats res) `shouldBe` Just rep

propWebSendsCommands :: forall f. TestFront f => Text -> Update f -> Property
propWebSendsCommands commandText update =
  isHelpUpdate @f update || isRepeatUpdate @f update ==> do
    let f = withHelpText commandText . withRepeatText commandText
    Right hm <- evalTBot @f f getHelpMessage
    Right rm <- evalTBot @f f getRepeatMessage
    Right url <-
      evalTBot @f (f . withUpdate update) $
        recieveActions >> prepareRequest @f update
    case toTestUpdate @f update of
      HelpUpdateT user -> HTTP.stringEncode hm `T.isInfixOf` url `shouldBe` True
      RepeatUpdateT user -> HTTP.stringEncode rm `T.isInfixOf` url `shouldBe` True

propWebSetsLastsRepeatition :: forall f. TestFront f => [Update f] -> Property
propWebSetsLastsRepeatition updates =
  any (isUpdateRepeatsUpdate @f) updates ==> do
    let us = map (toTestUpdate @f) $ filter (isUpdateRepeatsUpdate @f) updates
    finalState <-
      execTBot @f (withUpdates updates) $
        recieveActions @f @(TestBot f) >>= mapM_ (executeAction @f)
    foldl f M.empty us `shouldBe` bRepeats finalState
    foldl f M.empty us `shouldNotBe` M.empty
  where
    f map (UpdateRepeatsT userID rep) = M.insert userID rep map
