{-# LANGUAGE DeriveGeneric #-}
module MiniLight.Component.Internal.Types where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

data ComponentConfig = ComponentConfig {
  name :: T.Text,
  uid :: Maybe T.Text,
  properties :: Value
} deriving (Eq, Generic)

instance ToJSON ComponentConfig
instance FromJSON ComponentConfig

data AppConfig = AppConfig {
  app :: [ComponentConfig]
} deriving Generic

instance ToJSON AppConfig
instance FromJSON AppConfig
