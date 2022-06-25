{-# LANGUAGE ImportQualifiedPost #-}

module Mocks.TestFront where

import Bot.FrontEnd (Action, HasEnv (..), IsFrontEnd (..))
import Bot.IO (FrontEndIO (..))
import Bot.Types (Repeat)
import Control.Monad.State (gets, modify)
import Data.Functor ((<&>))
import Data.Map qualified as M
import Extended.Text (Text)
import Extended.Text qualified as T
import FrontEnd.Console.Main (Console, NotRequired (..))
import FrontEnd.Telegram.Internal qualified as TG
import FrontEnd.Telegram.Main (Telegram)
import FrontEnd.Telegram.Main qualified as TG
import FrontEnd.Vkontakte.Internal qualified as VK
import FrontEnd.Vkontakte.Main (Vkontakte)
import FrontEnd.Vkontakte.Main qualified as VK
import Mocks.Constants (defaultConsoleEnv, defaultHelpMessage, defaultRepeatMessage, defaultTGEnv, defaultVKEnv)
import Mocks.Gens (EchoUpdateGen, RepeatEchoUpdateGen)
import Mocks.TestBot (BotState (..), TestBot)
import Test.QuickCheck (Arbitrary)

class
  ( Arbitrary (Update f),
    Arbitrary (EchoUpdateGen f),
    Arbitrary (RepeatEchoUpdateGen f),
    Show (Update f),
    Show (EchoUpdateGen f),
    Show (RepeatEchoUpdateGen f),
    Ord (BotUser f),
    Show (Action f),
    Eq (Action f),
    IsFrontEnd f
  ) =>
  TestFront f
  where
  prepareRequest :: Update f -> TestBot f Text

  toTestUpdate :: Update f -> TestUpdate f

  initialFrontEnv :: BotFrontEnv f

instance TestFront Vkontakte where
  toTestUpdate = \case
    VK.EchoUpdate text userID [] ->
      EchoRepeatUpdateT text userID
    VK.EchoUpdate text userID as ->
      EchoUpdateT text userID
    VK.HelpUpdate userID ->
      HelpUpdateT userID
    VK.RepeatUpdate userID ->
      RepeatUpdateT userID
    VK.UpdateRepeats userID repeat ->
      UpdateRepeatsT userID repeat
    VK.Trash t -> TrashT t

  prepareRequest = VK.prepareRequest

  initialFrontEnv = defaultVKEnv

instance TestFront Telegram where
  toTestUpdate = \case
    TG.EchoUpdate _ userID chatID _ (Just text) ->
      EchoRepeatUpdateT text (TG.BotUser userID chatID)
    TG.EchoUpdate _ userID chatID _ _ ->
      EchoUpdateT "" (TG.BotUser userID chatID)
    TG.RepeatUpdate _ userID chatID ->
      RepeatUpdateT (TG.BotUser userID chatID)
    TG.HelpUpdate _ userID chatID ->
      HelpUpdateT (TG.BotUser userID chatID)
    TG.UpdateRepeats _ userID chatID _ rep ->
      UpdateRepeatsT (TG.BotUser userID chatID) rep
    TG.Trash _ obj ->
      TrashT $ T.show obj

  prepareRequest = TG.prepareRequest

  initialFrontEnv = defaultTGEnv

instance TestFront Console where
  toTestUpdate t
    | "/help" `T.isPrefixOf` t = HelpUpdateT NotRequired
    | "/repeat" `T.isPrefixOf` t = RepeatUpdateT NotRequired
    | otherwise = EchoRepeatUpdateT t NotRequired

  prepareRequest = pure

  initialFrontEnv = defaultConsoleEnv

data TestUpdate f
  = EchoUpdateT !Text (BotUser f)
  | EchoRepeatUpdateT !Text (BotUser f)
  | HelpUpdateT !(BotUser f)
  | RepeatUpdateT !(BotUser f)
  | UpdateRepeatsT !(BotUser f) !Repeat
  | TrashT !Text

deriving instance (Show (BotUser f)) => Show (TestUpdate f)

deriving instance (Eq (BotUser f)) => Eq (TestUpdate f)

getUserFromEchoUpdate :: forall f. TestFront f => Update f -> BotUser f
getUserFromEchoUpdate u = case toTestUpdate @f u of
  EchoRepeatUpdateT _ u -> u

instance TestFront f => FrontEndIO f (TestBot f) where
  getUpdates = gets bUpdates

  sendResponse resp =
    modify
      ( \BotState {..} ->
          BotState {bSenededResponse = resp : bSenededResponse, ..}
      )

instance TestFront f => HasEnv f (TestBot f) where
  getRepeats u = gets bRepeats <&> M.lookup u

  setRepeats u r = modify $
    \BotState {..} -> BotState {bRepeats = M.insert u r bRepeats, ..}

  defaultRepeats = gets bDefaultRepeats

  getFrontEnv = gets bFrontData

  setFrontEnv setEnv = modify $
    \BotState {..} -> BotState {bFrontData = setEnv bFrontData, ..}

  getHelpMessage = pure defaultHelpMessage

  getRepeatMessage = pure defaultRepeatMessage
