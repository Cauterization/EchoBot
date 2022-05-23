module FrontEnd.Vkontakte.Config
  ( VKConfig (..),
    VKGroup,
  )
where

import Bot.Types (ID (ID), Token)
import Dhall (FromDhall, Generic)

data VKGroup

data VKConfig = VKConfig
  { cToken :: Token,
    cGroupID :: ID VKGroup
  }
  deriving (Show, Generic, FromDhall)
