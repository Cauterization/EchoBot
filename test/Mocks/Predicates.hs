module Mocks.Predicates where

import Bot.FrontEnd (Update)
import Mocks.TestFront (TestFront (..), TestUpdate (..))

isRepeatEchoUpdate,
  isEchoUpdate,
  isHelpUpdate,
  isRepeatUpdate,
  isUpdateRepeatsUpdate ::
    forall f. TestFront f => Update f -> Bool
isRepeatEchoUpdate u = case toTestUpdate @f u of
  EchoRepeatUpdateT {} -> True
  _ -> False
isEchoUpdate u = case toTestUpdate @f u of
  EchoUpdateT {} -> True
  _ -> False
isHelpUpdate u = case toTestUpdate @f u of
  HelpUpdateT {} -> True
  _ -> False
isRepeatUpdate u = case toTestUpdate @f u of
  RepeatUpdateT {} -> True
  _ -> False
isUpdateRepeatsUpdate u = case toTestUpdate @f u of
  UpdateRepeatsT {} -> True
  _ -> False
