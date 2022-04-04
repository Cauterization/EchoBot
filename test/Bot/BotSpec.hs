{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.BotSpec
  ( spec,
  )
where

import Control.Monad.Catch
import Control.Monad.Except
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

data TestUpdate f
    = EchoUpdateT       !T.Text (BotUser f) 
    | EchoRepeatUpdateT !T.Text (BotUser f) 
    | HelpUpdateT       !(BotUser f)
    | RepeatUpdateT     !(BotUser f)
    | UpdateRepeatsT    !(BotUser f)  !Repeat
    | TrashT            !T.Text

deriving instance (Show (BotUser f)) => Show (TestUpdate f)
deriving instance (Eq (BotUser f)) => Eq (TestUpdate f)

testToken :: Text
testToken = "TestToken"

data BotState f = BotState
    { bUpdates :: [Update f]
    , bSenededResponse :: [Action f]
    , bRepeats :: M.Map (BotUser f) Repeat
    , bDefaultRepeats :: Repeat
    , bFrontData :: FrontData f
    }

initialState :: TestFront f => BotState f
initialState = BotState
    { bUpdates = []
    , bSenededResponse = []
    , bRepeats = M.empty
    , bDefaultRepeats = 1
    , bFrontData = mempty
    }

withUpdates :: [Update f] -> (BotState f -> BotState f)
withUpdates us BotState{..} = BotState{ bUpdates = bUpdates <> us, .. }

withFrontData :: FrontData f -> (BotState f -> BotState f)
withFrontData f BotState{..} = BotState{ bFrontData = f, .. }

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

instance MonadThrow (TestBot f) where
    throwM e = case fromException (SomeException e) of
        Just err -> throwError err
        _        -> error "non-bot exception."

instance MonadCatch (TestBot f) where
    catch = undefined

instance Logger.HasLogger (TestBot f) where
    mkLog v t = when (v >= Logger.Warning) $ tell [(v, t)]

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

class ( Arbitrary (Update f), Show (Update f)
      , Ord (BotUser f)
      , Show (Action f), Eq (Action f)
      , IsFrontEnd f
      , Monoid (WebOnly f URL)
      ) => TestFront f where
    onWeb :: a -> WebOnly f a
    toTestUpdate :: Update f -> TestUpdate f

runTBot :: forall f a. (TestFront f, FrontEndIO f (TestBot f))  
    => (BotState f -> BotState f) 
    -> TestBot f a
    -> IO (Either BotError a, BotState f)
runTBot f b = do

    either 
            (T.putStrLn  . T.show) 
            (mapM_ (T.putStrLn . (\(v, t) -> v >. ": " <> t)))
        $ flip evalState (f initialState) 
        $ runExceptT 
        $ fmap snd $ runWriterT $ unwrapTB $ 
            recieveActions @f @(TestBot f)

    pure $ flip runState (f initialState) 
        $ runExceptT $ fmap fst $ runWriterT $ unwrapTB b

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

    it "should echo any non-command input back" $ property $ propSendsEchoBack @f

        -- property $ \(update :: Update f) -> 
        --     isRepeatEchoUpdate @f update 
        --     || isEchoUpdate @f update 
        --     ==> do
        --         (res, _) <-  runTBot @f (withUpdates [update]) $
        --             recieveActions @f @(TestBot f)
        --         (eitherURL, _) <- runTBot @f id 
        --             (prepareRequest @f @(TestBot f) update)
        --         case toTestUpdate @f update of

        --             EchoRepeatUpdateT text userID -> case eitherURL of
        --                 Right url 
        --                     -> res `shouldBe` Right [SendRepeatEcho userID text url]
        --                 Left err 
        --                     -> Left err `shouldBe` Right "url"

        --             EchoUpdateT text userID -> case eitherURL of
        --                 Right url 
        --                     -> res `shouldBe` Right [SendEcho userID text url]
        --                 Left err 
        --                     -> Left err `shouldBe` Right "url"

propSendsEchoBack :: forall f. TestFront f => Update f -> Property
propSendsEchoBack update = 
    isRepeatEchoUpdate @f update || isEchoUpdate @f update 
        ==> do
            (res, _) <-  runTBot @f (withUpdates [update]) $
                recieveActions @f @(TestBot f)
            (eitherURL, _) <- runTBot @f id 
                (prepareRequest @f @(TestBot f) update)
            case toTestUpdate @f update of
                     
                EchoRepeatUpdateT text userID -> case eitherURL of
                    Right url 
                        -> res `shouldBe` Right [SendRepeatEcho userID text url]
                    Left err 
                        -> Left err `shouldBe` Right "url"
             
                EchoUpdateT text userID -> case eitherURL of
                    Right url 
                        -> res `shouldBe` Right [SendEcho userID text url]
                    Left err 
                        -> Left err `shouldBe` Right "url"


specWeb :: forall f. (TestFront f, FrontEndIO f (TestBot f)) => Spec
specWeb = describe (frontName @f <> " web tests:") $ do

    it "should update counter when it recieves updateRepeats update" $ do

        property $ \(update :: Update f) ->
            isUpdateRepeatsUpdate @f update ==> do
                (_, res) <-  runTBot @f (withUpdates [update]) $
                    recieveActions @f @(TestBot f) >>= mapM_ (executeAction @f)
                case toTestUpdate @f update of

                    UpdateRepeatsT user rep -> 
                        M.lookup user (bRepeats res) `shouldBe` Just rep

specConsole :: Spec
specConsole = describe "Console specific tests:" $ do

    it "should update counter when it recieves correct update and it awaits it" $ do

        property $ forAll (chooseInt (1, 5)) $ \i -> do
            
            (_, res) <- runTBot @Console (
                withUpdates [T.show  i] . 
                withFrontData (ConsoleAwaits True)) $
                    recieveActions @Console @(TestBot Console) 
                    >>= mapM_ (executeAction @Console)
            M.lookup NotRequired (bRepeats res) `shouldBe` Just (Repeat i)

    -- it "should'nt do it otherwise" $ do

    --     property $ do
    --         update <- T.show <$> chooseInt (1, 5)
    --         (_, res) <- runTBot @Console (
    --             withUpdates [update] . 
    --             withFrontData (ConsoleAwaits False)) $
    --                 recieveActions @Console @(TestBot Console) 
    --                 >>= mapM_ (executeAction @Console)
    --         M.lookup NotRequired (bRepeats res) `shouldBe` T.read update


isRepeatEchoUpdate :: forall f. TestFront f => Update f -> Bool
isRepeatEchoUpdate u = case toTestUpdate @f u of
    EchoRepeatUpdateT{} -> True
    _ -> False

isEchoUpdate :: forall f. TestFront f => Update f -> Bool
isEchoUpdate u = case toTestUpdate @f u of
    EchoUpdateT{} -> True
    _ -> False

isUpdateRepeatsUpdate :: forall f. TestFront f => Update f -> Bool
isUpdateRepeatsUpdate u = case toTestUpdate @f u of
    UpdateRepeatsT{} -> True
    _ -> False

isCorrectConsoleNewRepeatsInput :: Text -> Bool
isCorrectConsoleNewRepeatsInput input = case T.readEither @Repeat input of
    Right r -> r < 5 && r > 1
    Left err -> False

--     it "should output menu for /repeat command" $ do
--       let config = stubConfig
--       let h = handleWith config
--       responses <- runBotWithConfig config $ respond h (MessageEvent "/repeat")
--       responses `shouldSatisfy` any isMenuResponse

--     it "should keep the repetition count set by the user" $ do
--       let comment = "comment"
--       let config = stubConfig {confRepetitionCount = 1}
--       let newRepCount = 3
--       let h = handleWith config
--       responses <- runBotWithConfig config $ do
--         [MenuResponse _ opts] <- respond h $ MessageEvent "/repeat"
--         Just request <- pure $ lookup newRepCount opts
--         _ <- respond h request
--         respond h $ MessageEvent comment
--       responses `shouldBe` replicate newRepCount (MessageResponse comment)

--     it "should output the help text for /help command" $ do
--       let helpText = "My help text"
--       let config = stubConfig {confHelpReply = helpText}
--       let h = handleWith config
--       responses <- runBotWithConfig config $ respond h $ MessageEvent "/help"
--       responses `shouldBe` [MessageResponse helpText]

--     it "should output the predefined menu title for /repeat command" $ do
--       let title = "My title"
--       let config = stubConfig {confRepeatReply = title}
--       let h = handleWith config
--       responses <- runBotWithConfig config $ respond h $ MessageEvent "/repeat"
--       responses `shouldSatisfy` any (isMenuResponseWithTitle title)

--     it "should substitute {count} with repetition count in the menu title" $ do
--       let config =
--             stubConfig
--               { confRepeatReply = "My count is {count}, {right}.",
--                 confRepetitionCount = 3
--               }
--       let h = handleWith config
--       responses <- runBotWithConfig config $ respond h $ MessageEvent "/repeat"
--       responses `shouldSatisfy` any (isMenuResponseWithTitle "My count is 3, {right}.")

--     it "should not recognize an unknown command" $ do
--       shouldNotRecognizeHelpCommand "/xhelp"
--       shouldNotRecognizeHelpCommand "/ help"
--       shouldNotRecognizeHelpCommand "/helpx"
--       shouldNotRecognizeHelpCommand "/he lp"
--       shouldNotRecognizeHelpCommand "x/help"
--       shouldNotRecognizeHelpCommand "x /help"

-- isMenuResponse :: Response T.Text -> Bool
-- isMenuResponse (MenuResponse _ _) = True
-- isMenuResponse _ = False

-- isMenuResponseWithTitle :: T.Text -> Response T.Text -> Bool
-- isMenuResponseWithTitle title (MenuResponse t _) = title == t
-- isMenuResponseWithTitle _ _ = False

-- shouldNotRecognizeHelpCommand :: T.Text -> Expectation
-- shouldNotRecognizeHelpCommand = shouldRecognizeHelpCommandOrNot False

-- shouldRecognizeHelpCommandOrNot :: Bool -> T.Text -> Expectation
-- shouldRecognizeHelpCommandOrNot matchOrNot input = do
--   let helpText = "Help text, not " <> input
--   let config = stubConfig {confHelpReply = helpText}
--   let h = handleWith config
--   response <- runBotWithConfig config $ respond h $ MessageEvent input
--   let expected = [MessageResponse helpText]
--   if matchOrNot
--     then response `shouldBe` expected
--     else response `shouldNotBe` expected

-- hasCommandPrefix :: String -> Bool
-- hasCommandPrefix (' ' : xs) = hasCommandPrefix xs
-- hasCommandPrefix ('/' : _) = True
-- hasCommandPrefix _ = False

-- runBotWithConfig :: Config -> Interp a -> IO a
-- runBotWithConfig config = runBot (stateWith config)

-- runBot :: State -> Interp a -> IO a
-- runBot s0 m = do
--   (a, logMessages) <- S.evalStateT (runWriterT m) s0
--   logMessages `shouldSatisfy` null
--   pure a

-- handleWith :: Config -> Handle Interp T.Text
-- handleWith config =
--   Handle
--     { hGetState = S.get,
--       hModifyState' = S.modify',
--       hLogHandle = logHandle,
--       hConfig = config,
--       hTextFromMessage = Just,
--       hMessageFromText = id
--     }

-- logHandle :: Logger.Handle Interp
-- logHandle =
--   Logger.Handle
--     { Logger.hLowLevelLog =
--         \level text -> when (level >= Logger.Warning) $ tell [(level, text)]
--     }

-- stubConfig :: Config
-- stubConfig =
--   Config
--     { confRepeatReply = T.empty,
--       confHelpReply = T.empty,
--       confRepetitionCount = 1
--     }

-- stateWith :: Config -> State
-- stateWith = either (error . T.unpack) id . makeState