{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# Options_GHC -Wno-incomplete-patterns #-}

module Bot.BotSpec
  ( spec,
  )
where

import Control.Monad.Catch
    ( Exception(fromException),
      SomeException(SomeException),
      MonadCatch(..),
      MonadThrow(..) )
import Control.Monad.Except
    ( join, when, ExceptT(..), runExceptT, MonadError(throwError) )
import Control.Monad.State
import Control.Monad.Writer 

import Data.Functor
import Data.Map qualified as M

import Extended.Text (Text)

import Logger.Handle qualified as Logger
import Logger.Handle ((>.), (.<))

import Test.Hspec 
import Test.QuickCheck

import Bot.FrontEnd
import Bot.Bot
import Bot.Error
import Bot.IO

import Vkontakte.FrontEnd (Vkontakte)
import Vkontakte.FrontEnd qualified as VK
import Vkontakte.Internal qualified as VK

import Telegram.FrontEnd (Telegram)
import Telegram.FrontEnd qualified as TG
import Telegram.Internal qualified as TG

import Console.FrontEnd (Console, ConsoleAwaits(..))

import qualified Extended.HTTP as HTTP
import qualified Extended.Text as T

import Bot.Types
import Data.Either
import Data.Aeson
import Data.String

import System.IO.Unsafe
import Data.List
import Control.Monad.Extra (whenM)

spec :: Spec
spec = do
    specFront   @Vkontakte
    specWeb     @Vkontakte
    specFront   @Telegram
    specWeb     @Telegram
    specFront   @Console
    specConsole 

specFront :: forall f. (TestFront f, FrontEndIO f (TestBot f)) => Spec
specFront = describe (frontName @f <> " common tests:") $ do

    it "should echo any non-command input back" 
        $ property $ propSendsEchoBack @f

propSendsEchoBack :: forall f. TestFront f => Update f -> Property
propSendsEchoBack update = 
    isRepeatEchoUpdate @f update || isEchoUpdate @f update ==> do
        res <- evalTBot @f (withUpdate update) $
            recieveActions @f @(TestBot f)
        eitherURL <- evalTBot @f id 
            (prepareRequest @f @(TestBot f) update)
        case toTestUpdate @f update of
            EchoRepeatUpdateT text userID -> case eitherURL of
                Right url 
                    -> res `shouldBe` Right [SendRepeatEcho userID text url]

         
            EchoUpdateT text userID -> case eitherURL of
                Right url 
                    -> res `shouldBe` Right [SendEcho userID text url]

specWeb :: forall f. (TestFront f, FrontEndIO f (TestBot f)) => Spec
specWeb = describe (frontName @f <> " web tests:") $ do

    it "should update counter when it recieves updateRepeats update" 
        $ property $ propWebUpdatesCounter @f

    it "should send corresponding messages according to recieved commands"
        $ property $ propWebSendsCommands @f

    it "this messages are percent-encoded"
        $ property $ join $ \text update -> do
            HTTP.stringEncode text /= text ==> propWebSendsCommands @f text

    it "only last taken repeatition uis taken into account"
        $ property $ withMaxSuccess 40 $ propWebSetsLastsRepeatition @f

propWebUpdatesCounter :: forall f. TestFront f => Update f -> Property
propWebUpdatesCounter update = 
    isUpdateRepeatsUpdate @f update ==> do
        res <-  execTBot @f (withUpdates [update]) $
            recieveActions @f @(TestBot f) >>= mapM_ (executeAction @f)
        case toTestUpdate @f update of
            UpdateRepeatsT user rep -> 
                M.lookup user (bRepeats res) `shouldBe` Just rep

propWebSendsCommands :: forall f. TestFront f => Text -> Update f -> Property
propWebSendsCommands commandText update = 
    isHelpUpdate @f update || isRepeatUpdate @f update ==> do
        let f = withHelpText commandText . withRepeatText commandText
        Right hm  <- evalTBot @f f getHelpMessage 
        Right rm  <- evalTBot @f f getRepeatMessage
        Right url <- evalTBot @f (f . withUpdate update) $
            recieveActions >> prepareRequest @f update
        case toTestUpdate @f update of
            HelpUpdateT   user -> HTTP.stringEncode hm `T.isInfixOf` url `shouldBe` True
            RepeatUpdateT user -> HTTP.stringEncode rm `T.isInfixOf` url `shouldBe` True

propWebSetsLastsRepeatition :: forall f. TestFront f => [Update f] -> Property
propWebSetsLastsRepeatition updates = 
    any (isUpdateRepeatsUpdate @f) updates ==> do
        let us = map (toTestUpdate @f) $ filter (isUpdateRepeatsUpdate @f) updates
        finalState <- execTBot @f (withUpdates updates) $
            recieveActions @f @(TestBot f) >>= mapM_ (executeAction @f)
        foldl f M.empty us `shouldBe` bRepeats finalState
        foldl f M.empty us `shouldNotBe` M.empty
  where
    f map (UpdateRepeatsT userID rep) = M.insert userID rep map

specConsole :: Spec
specConsole = describe "Console specific tests:" $ do

    it "should update counter when it recieves correct update and it awaits of it" $ do

        property $ forAll (chooseInt (1, 5)) $ 
            propConsoleUpdatesCounter (ConsoleAwaits True)

    it "should'nt do it otherwise" $ do

        property $ forAll (chooseInt (1, 5)) $ 
            propConsoleUpdatesCounter (ConsoleAwaits False)

    it "should handle incorrect input" $ do

        property $ \i -> propConsoleUpdatesIncorrectCounter i

    it "should send corresponding messages according to recieved commands" $ do

        property $ forAll (elements ["/help", "/repeat"]) propConsoleSendsCommands

propConsoleUpdatesCounter :: ConsoleAwaits -> Int -> Property
propConsoleUpdatesCounter awaits repeat = repeat < 5 && repeat > 0 ==> do
    res <- execTBot (
        withUpdates [T.show  repeat] . 
        withFrontData awaits) $
            recieveActions @Console 
            >>= mapM_ executeAction
    if unAwaits awaits 
    then M.lookup NotRequired (bRepeats res) `shouldBe` Just (Repeat repeat)
    else bRepeats res `shouldBe` bRepeats (initialState @Console)

propConsoleUpdatesIncorrectCounter :: Text -> Property
propConsoleUpdatesIncorrectCounter input = 
    let err = eitherDecode @Repeat $ fromString $ T.unpack input in
    isLeft err ==> do
        res <- evalTBot @Console (
            withUpdates [input] . 
            withFrontData (ConsoleAwaits True)) 
            $
                recieveActions 
                >>= mapM_ executeAction 
        res `shouldBe` Left (ParsingError $ T.pack $ fromLeft "" err)

propConsoleSendsCommands :: Text -> Property
propConsoleSendsCommands input = 
    input == "/help" || input == "/repeat" ==> do
        Right hm  <- evalTBot @Console id getHelpMessage 
        Right rm  <- evalTBot @Console id getRepeatMessage
        Right [a] <- evalTBot @Console (withUpdate input) 
            recieveActions 
        a `shouldBe`  case input of
                "/help"   -> SendHelpMessage   NotRequired hm
                "/repeat" -> SendRepeatMessage NotRequired rm

newtype TestBot f a = TestBot 
    {unwrapTB :: 
    WriterT [(Logger.Verbosity, Text)] 
    (ExceptT BotError  
    (State (BotState f))) a }
    deriving newtype 
        ( Functor
        , Applicative
        , Monad
        , MonadWriter [(Logger.Verbosity, Text)]
        , MonadError BotError
        , MonadState (BotState f))

runTBot :: forall f a. (TestFront f, FrontEndIO f (TestBot f))  
    => (BotState f -> BotState f) 
    -> TestBot f a
    -> IO (Either BotError a, BotState f)
runTBot f b = do
    either 
            (pure . const ()) 
            (mapM_ (T.putStrLn . (\(v, t) -> v >. ": " <> t)))
        $ flip evalState (f initialState) 
        $ runExceptT 
        $ fmap snd $ runWriterT $ unwrapTB $ 
            recieveActions @f @(TestBot f)

    pure $ flip runState (f initialState) 
        $ runExceptT $ fmap fst $ runWriterT $ unwrapTB b

execTBot :: forall f a. (TestFront f, FrontEndIO f (TestBot f))
    => (BotState f -> BotState f) 
    -> TestBot f a
    -> IO (BotState f)
execTBot f b = snd <$> runTBot f b 

evalTBot :: forall f a. (TestFront f, FrontEndIO f (TestBot f))
    => (BotState f -> BotState f) 
    -> TestBot f a
    -> IO (Either BotError a)
evalTBot f b = fst <$> runTBot f b

testToken :: Text
testToken = "TestToken"

data BotState f = BotState
    { bUpdates :: [Update f]
    , bSenededResponse :: [Action f]
    , bRepeats :: M.Map (BotUser f) Repeat
    , bDefaultRepeats :: Repeat
    , bFrontData :: FrontData f
    , bHelpMessage :: Text
    , bRepeatMessage :: Text
    , bIsLogerRequired :: Bool
    }

initialState :: TestFront f => BotState f
initialState = BotState
    { bUpdates = []
    , bSenededResponse = []
    , bRepeats = M.empty
    , bDefaultRepeats = 1
    , bFrontData = mempty
    , bHelpMessage   = "HelpMessage"
    , bRepeatMessage = "RepeatMessage"
    , bIsLogerRequired = True
    }

withUpdate :: Update f -> (BotState f -> BotState f)
withUpdate = withUpdates . pure

withUpdates :: [Update f] -> (BotState f -> BotState f)
withUpdates us BotState{..} = BotState{ bUpdates = bUpdates <> us, .. }

withFrontData :: FrontData f -> (BotState f -> BotState f)
withFrontData f BotState{..} = BotState{ bFrontData = f, .. }

withHelpText, withRepeatText :: Text -> (BotState f -> BotState f)
withHelpText   help   BotState{..} =  BotState{ bHelpMessage = help, .. }
withRepeatText repeat BotState{..} =  BotState{ bRepeatMessage = repeat, .. }

woLogging :: BotState f -> BotState f
woLogging BotState{..} = BotState{ bIsLogerRequired = False, .. }

instance MonadThrow (TestBot f) where
    throwM e = case fromException (SomeException e) of
        Just err -> throwError err
        _        -> error "non-bot exception."

instance MonadCatch (TestBot f) where
    catch = undefined

instance Logger.HasLogger (TestBot f) where
    mkLog v t = whenM (gets bIsLogerRequired) 
              $ when (v >= Logger.Warning) 
              $ tell [(v, t)]

instance FrontEndIO f (TestBot f) where

    getUpdates = gets bUpdates

    sendResponse _ = pure ()

    sendWebResponse _ = pure ()

instance TestFront f => HasEnv f (TestBot f) where

    getRepeats u = gets bRepeats <&> M.lookup u

    setRepeats u r = modify $
        \BotState{..} -> BotState {bRepeats = M.insert u r bRepeats, ..}

    defaultRepeats = gets bDefaultRepeats

    getFrontData = gets bFrontData
        
    setFrontData fd = modify $
        \BotState{..} -> BotState {bFrontData = fd <> bFrontData, ..}

    getToken = pure $ onWeb @f $ Token @f "BotToken"

    getPollingTime = pure $ onWeb @f (45 :: Int)

    getHelpMessage = pure "HelpMessage"

    getRepeatMessage = pure "RepeatMessage"

class ( Arbitrary (Update f), Show (Update f)
      , Ord (BotUser f)
      , Show (Action f), Eq (Action f)
      , IsFrontEnd f
      , Monoid (WebOnly f URL)
      ) => TestFront f where

    onWeb :: a -> WebOnly f a

    toTestUpdate :: Update f -> TestUpdate f

instance TestFront Vkontakte where

    onWeb = id

    toTestUpdate = \case

        VK.EchoUpdate text userID []
            -> EchoRepeatUpdateT text userID

        VK.EchoUpdate text userID as
            -> EchoUpdateT text userID

        VK.HelpUpdate userID 
            -> HelpUpdateT userID

        VK.RepeatUpdate userID 
            -> RepeatUpdateT userID

        VK.UpdateRepeats userID repeat 
            -> UpdateRepeatsT userID repeat

        VK.Trash t -> TrashT t

instance TestFront Telegram where

    onWeb = id

    toTestUpdate = \case

        TG.EchoUpdate _ userID chatID _ (Just text)
            -> EchoRepeatUpdateT text (TG.BotUser userID chatID)

        TG.EchoUpdate _ userID chatID _ _
            -> EchoUpdateT "" (TG.BotUser userID chatID)

        TG.RepeatUpdate _ userID chatID
            -> RepeatUpdateT (TG.BotUser userID chatID)

        TG.HelpUpdate _ userID chatID
            -> HelpUpdateT (TG.BotUser userID chatID)

        TG.UpdateRepeats _ userID chatID _ rep
            -> UpdateRepeatsT (TG.BotUser userID chatID) rep

        TG.Trash _ obj
            -> TrashT $ T.show obj

instance TestFront Console where

    onWeb _ = NotRequired

    toTestUpdate t
        | "/help"   `T.isPrefixOf` t  = HelpUpdateT   NotRequired
        | "/repeat" `T.isPrefixOf` t  = RepeatUpdateT NotRequired
        | otherwise                   = EchoRepeatUpdateT t NotRequired 
        
data TestUpdate f
    = EchoUpdateT       !T.Text (BotUser f) 
    | EchoRepeatUpdateT !T.Text (BotUser f) 
    | HelpUpdateT       !(BotUser f)
    | RepeatUpdateT     !(BotUser f)
    | UpdateRepeatsT    !(BotUser f)  !Repeat
    | TrashT            !T.Text

deriving instance (Show (BotUser f)) => Show (TestUpdate f)
deriving instance (Eq (BotUser f)) => Eq (TestUpdate f)

isRepeatEchoUpdate, isEchoUpdate, isHelpUpdate, isRepeatUpdate, isUpdateRepeatsUpdate 
    :: forall f. TestFront f => Update f -> Bool    

isRepeatEchoUpdate u = case toTestUpdate @f u of
    EchoRepeatUpdateT{} -> True
    _ -> False

isEchoUpdate u = case toTestUpdate @f u of
    EchoUpdateT{} -> True
    _ -> False

isHelpUpdate u = case toTestUpdate @f u of
    HelpUpdateT{} -> True
    _ -> False

isRepeatUpdate u = case toTestUpdate @f u of
    RepeatUpdateT{} -> True
    _ -> False

isUpdateRepeatsUpdate u = case toTestUpdate @f u of
    UpdateRepeatsT{} -> True
    _ -> False
