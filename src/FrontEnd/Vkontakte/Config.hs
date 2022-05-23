module FrontEnd.Vkontakte.Config
  ( VKConfig (..),
  )
where

import Bot.Types (Token)
import Dhall (FromDhall, Generic)

data VKConfig = Config
  { cToken :: Token
  }
  deriving (Show, Generic, FromDhall)
