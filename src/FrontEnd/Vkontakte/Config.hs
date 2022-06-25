module FrontEnd.Vkontakte.Config
  ( VKConfig (..)
  )
where

import Bot.Types (Token)
import Dhall (FromDhall, Generic)

data VKConfig = VKConfig
  { cToken :: Token,
    cGroupID :: Integer
  }
  deriving (Show, Generic, FromDhall)
