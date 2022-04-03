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
import Console.FrontEnd (Console)
import qualified Extended.HTTP as HTTP
import qualified Extended.Text as T
import qualified Data.Map as M
import Bot.Types


testToken :: Text
testToken = 


data BotResponse f
    = EchoR Text 
    deriving (Show, Eq)

-- data UpdateType f
--     = EchoU
--     | EchoSingleU 
--     | HelpU 
--     | RepeatU 
--     | UpdateRepestsU
--     | Trash


data Env f = Env
    { envLogger         :: !(Logger.Logger IO)
    , envDefaultRepeats :: !Repeat    
    , envHelpMessage    :: !Text
    , envRepeatMessage  :: !Text
    , envRepeats        :: !(IORef (M.Map (BotUser f) Repeat))
    , envToken          :: !(WebOnly f (Token f))
    , envFrontData      :: !(IORef (FrontData f))
    , envPollingTime    :: !(WebOnly f PollingTime)
    }

data BotState f = BotState
    { bUpdates :: [Update f]
    , bSenededResponse :: [BotResponse f]
    , bRepeats :: M.Map (BotUser f) Repeat
    }

initialState :: BotState f
initialState = BotState
    { bUpdates = []
    , bSenededResponse = []
    }

withUpdates :: [Update f] -> (BotState f -> BotState f)
withUpdates us BotState{..} =  BotState{ bUpdates = bUpdates <> us}

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

instance {-# OVERLAPS #-} FrontEndIO f (TestBot f) where
    getUpdates = gets bUpdates

instance HasEnv f (TestBot f) where
    getRepeats = gets 
    getToken = "TestToken"


-- class HasEnv f m | m -> f where
--     getRepeats       :: BotUser f -> m (Maybe Repeat)
--     setRepeats       :: BotUser f -> Repeat -> m ()
--     defaultRepeats   :: m Repeat
--     getFrontData     :: m (FrontData f)
--     setFrontData     :: FrontData f -> m ()
--     getToken         :: m (WebOnly f (Token f))
--     getPollingTime   :: m (WebOnly f PollingTime)
--     getHelpMessage   :: m Text
--     getRepeatMessage :: m Text

-- instance {-# OVERLAPS #-} HTTP.MonadHttp (TestBot f)

class (Arbitrary (Update f), Show (Update f), IsFrontEnd f)
    => TestFront f where
    -- getUpdateType :: Update f -> UpdateType f
    getTextFromUpdate :: Update f -> Text
    responseFromUpdate :: Update f -> [BotResponse f]

runTestBot :: forall f. IsFrontEnd f => (BotState f -> BotState f) -> IO [BotResponse f]
runTestBot f = do

    either 
            (T.putStrLn  . T.show) 
            (mapM_ (T.putStrLn . (\(v, t) -> v >. ": " <> t)))
        $ flip evalState (f initialState) 
        $ runExceptT 
        $ fmap snd $ runWriterT $ unwrapTB $ bot @f

    pure $ bSenededResponse $ flip execState (f initialState) 
        $ runExceptT $ fmap fst $ runWriterT $ unwrapTB $ bot @f

instance TestFront Vkontakte where

    -- getUpdateType = \case
    --     VK.RepeatUpdate{} -> RepeatU
    --     VK.HelpUpdate{} -> HelpU
    --     VK.AttachmentUpdate{} -> EchoSingleU
    --     VK.Update VK.Message{..} -> EchoU
    --     VK.UpdateRepeats{} -> UpdateRepestsU
    --     VK.Trash t -> Trash

    getTextFromUpdate = \case
        VK.Update VK.Message{..} -> text

    responseFromUpdate = \case
        -- VK.RepeatUpdate{} -> RepeatU
        -- VK.HelpUpdate{} -> HelpU
        -- VK.AttachmentUpdate{} -> EchoSingleU
        VK.Update VK.Message{..} -> [EchoR text]
        -- VK.UpdateRepeats{} -> UpdateRepestsU
        -- VK.Trash t -> Trash


spec :: Spec
spec = specFront @Vkontakte

specFront :: forall f. TestFront f => Spec
specFront = do
    it "should echo any non-command input back" $ do
        property $ \(update :: Update f) ->
            isEchoUpdate @f update ==> do
            res <- runTestBot @f (withUpdates [update]) 
            res `shouldBe` [EchoR (getTextFromUpdate @f update)]

isEchoUpdate :: forall f. TestFront f => Update f -> Bool
isEchoUpdate u = case responseFromUpdate @f u of
    [EchoR{}] -> True
    _     -> False

-- import Control.Monad (when)
-- import qualified Control.Monad.State as S
-- import Control.Monad.Writer (WriterT, runWriterT, tell)
-- import qualified Data.Text as T
-- import Bot (Config (..), Event (..), Handle (..), Response (..), State, makeState, respond)
-- import Logger.Handle qualified as Logger


-- type Interp = WriterT [(Logger.Level, T.Text)] (S.StateT State IO)

-- spec :: Spec
-- spec =
--   {- HLINT ignore spec "Reduce duplication" -}
--   describe "respond" $ do
--     it "should echo any non-command input back" $
--       property $ \str ->
--         not (hasCommandPrefix str) ==> do
--           let comment = T.pack str
--           let config = stubConfig
--           let h = handleWith config
--           responses <-
--             runBotWithConfig config $ respond h (MessageEvent comment)
--           responses `shouldBe` [MessageResponse comment]

--     it "should echo a simple message for any specified amount of times specified in the config" $
--       property $ \(NonNegative count) -> do
--         let comment = "comment"
--         let config = stubConfig {confRepetitionCount = count}
--         let h = handleWith config
--         responses <- runBotWithConfig config $ respond h (MessageEvent comment)
--         responses `shouldBe` replicate count (MessageResponse comment)

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