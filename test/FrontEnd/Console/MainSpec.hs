{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FrontEnd.Console.MainSpec where

import Bot.Bot (executeAction, recieveActions)
import Bot.Error (BotError (..))
import Bot.FrontEnd (Action (..), HasEnv (..))
import Bot.Types (Repeat (..))
import Control.Monad (liftM3)
import Data.Aeson (eitherDecode)
import Data.Either (fromLeft, isLeft)
import Data.Map qualified as M
import Data.String (fromString)
import Extended.Text (Text)
import Extended.Text qualified as T
import FrontEnd.Console.Env (ConsoleEnv (..))
import FrontEnd.Console.Main (Console, NotRequired (..))
import Mocks.Run (evalTBot, execTBot, initialState)
import Mocks.TestBot (BotState (..))
import Mocks.With (withFrontData, withUpdate, withUpdates)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Test.QuickCheck
  ( Property,
    Testable (property),
    chooseInt,
    elements,
    forAll,
    (==>),
  )

spec :: Spec
spec = describe "Console specific tests:" $ do
  it "should update counter when it recieves correct update and it awaits of it" $ do
    property $
      forAll (chooseInt (1, 5)) $
        propConsoleUpdatesCounter True

  it "shouldn't do it otherwise" $ do
    property $
      forAll (chooseInt (1, 5)) $
        propConsoleUpdatesCounter False

  it "should handle incorrect input" $ do
    property $ propConsoleUpdatesIncorrectCounter

  it "should send corresponding messages according to recieved commands" $ do
    property $ forAll (elements ["/help", "/repeat"]) propConsoleExecutesCommands

propConsoleUpdatesCounter :: Bool -> Int -> Property
propConsoleUpdatesCounter awaits repeat =
  repeat < 5 && repeat > 0 ==> do
    res <-
      execTBot
        ( withUpdate (T.show repeat)
            . withFrontData (ConsoleEnv awaits)
        )
        $ recieveActions @Console
          >>= mapM_ executeAction
    if awaits
      then M.lookup NotRequired (bRepeats res) `shouldBe` Just (Repeat repeat)
      else bRepeats res `shouldBe` bRepeats (initialState @Console)

propConsoleUpdatesIncorrectCounter :: Text -> Property
propConsoleUpdatesIncorrectCounter input =
  let err = eitherDecode @Repeat $ fromString $ T.unpack input
   in isLeft err ==> do
        res <-
          evalTBot @Console
            ( withUpdates [input]
                . withFrontData (ConsoleEnv True)
            )
            $ recieveActions
              >>= mapM_ executeAction
        res `shouldBe` Left (ParsingError $ T.pack $ fromLeft "" err)

propConsoleExecutesCommands :: Text -> Property
propConsoleExecutesCommands input =
  input == "/help" || input == "/repeat" ==> do
    Right (hm, rm, [a]) <-
      evalTBot @Console (withUpdate input) $
        liftM3 (,,) getHelpMessage getRepeatMessage recieveActions
    a `shouldBe` case input of
      "/help" -> SendHelpMessage NotRequired hm
      "/repeat" -> SendRepeatMessage NotRequired rm
