{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mocks.Gens where

import Bot.FrontEnd (Update)
import FrontEnd.Console.Main (Console)
import FrontEnd.Telegram.Internal qualified as TG
import FrontEnd.Telegram.Main (Telegram)
import FrontEnd.Vkontakte.Internal qualified as VK
import FrontEnd.Vkontakte.Main (Vkontakte)
import Test.QuickCheck (Arbitrary (..), chooseInt)

newtype EchoUpdateGen f = EchoUpdateGen {unEchoUpdateGen :: Update f}

deriving newtype instance Show (Update f) => Show (EchoUpdateGen f)

instance Arbitrary (EchoUpdateGen Vkontakte) where
  arbitrary = do
    chooseInt (1, 2) >>= \case
      1 ->
        fmap EchoUpdateGen $
          VK.EchoUpdate
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
      2 -> EchoUpdateGen . unRepeatEchoUpdateGen <$> arbitrary @(RepeatEchoUpdateGen Vkontakte)

instance Arbitrary (EchoUpdateGen Telegram) where
  arbitrary = do
    chooseInt (1, 2) >>= \case
      1 ->
        fmap EchoUpdateGen $
          TG.EchoUpdate
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
      2 -> EchoUpdateGen . unRepeatEchoUpdateGen <$> arbitrary @(RepeatEchoUpdateGen Telegram)

deriving newtype instance Arbitrary (EchoUpdateGen Console)

newtype RepeatEchoUpdateGen f = RepeatEchoUpdateGen {unRepeatEchoUpdateGen :: Update f}

deriving newtype instance Show (Update f) => Show (RepeatEchoUpdateGen f)

instance Arbitrary (RepeatEchoUpdateGen Vkontakte) where
  arbitrary = fmap RepeatEchoUpdateGen $ VK.EchoUpdate <$> arbitrary <*> arbitrary <*> pure []

instance Arbitrary (RepeatEchoUpdateGen Telegram) where
  arbitrary =
    fmap RepeatEchoUpdateGen $
      TG.EchoUpdate
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (Just <$> arbitrary)

deriving newtype instance Arbitrary (RepeatEchoUpdateGen Console)
