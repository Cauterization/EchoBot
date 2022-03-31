module Bot.Main where

import Data.Foldable

import           Control.Exception


import Data.Aeson
import Data.ByteString.Lazy qualified as BL

import System.Exit qualified as Sys
import           Bot.App 
import           Bot.Config 
import Logger.IO qualified
import Control.Monad ((>=>))

-- import qualified FrontEnd.VK.Main  as VK
-- import qualified Messanger.TG.Main  as TG

run :: IO ()
run = handle h $ withConfig "config.json" runApp
  where
    h (e :: IOException) = Sys.die $ show e <> ". Terminating..."

runApp :: Show (Config f) => Config f -> IO ()
runApp c = print $ c

