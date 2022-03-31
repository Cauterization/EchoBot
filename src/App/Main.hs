module App.Main where

import Data.Foldable

import           Control.Exception


import Data.Aeson
import Data.ByteString.Lazy qualified as BL

import System.Exit qualified as Sys
import           App.App 
import           App.Config 
import Logger.IO qualified
import Control.Monad ((>=>))

-- import qualified FrontEnd.VK.Main  as VK
-- import qualified Messanger.TG.Main  as TG

run :: IO ()
run = handle (\(e :: IOException) -> Sys.die $ show e <> ". Terminating...") 
    $ chooseFront "config.json"

