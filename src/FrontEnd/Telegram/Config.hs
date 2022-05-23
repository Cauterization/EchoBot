module FrontEnd.Telegram.Config
  ( TGConfig (..),
  )
where

import Bot.Types (Token)
import Dhall (FromDhall, Generic)

data TGConfig = Config
  { cToken :: Token
  }
  deriving (Show, Generic, FromDhall)
