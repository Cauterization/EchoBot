{-# LANGUAGE TemplateHaskell #-}

module FrontEnd.Console.Env where

import Control.Lens (makeLenses)

data ConsoleEnv = ConsoleEnv
  { _consoleAwaitsRepUpdate :: !Bool
  }
  deriving (Show)

makeLenses ''ConsoleEnv
