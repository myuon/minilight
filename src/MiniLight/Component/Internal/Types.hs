{-# LANGUAGE DeriveGeneric #-}
module MiniLight.Component.Internal.Types where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.UUID
import qualified Data.UUID.V4
import GHC.Generics
import MiniLight.Component.Types
import MiniLight.Light

data ComponentConfig = ComponentConfig {
  name :: T.Text,
  uid :: Maybe T.Text,
  properties :: Value
} deriving (Eq, Show, Generic)

instance ToJSON ComponentConfig
instance FromJSON ComponentConfig

data AppConfig = AppConfig {
  app :: [ComponentConfig]
} deriving (Eq, Show, Generic)

instance ToJSON AppConfig
instance FromJSON AppConfig

-- | The type for component resolver
type Resolver
  = T.Text  -- ^ Component Type
  -> Value  -- ^ Component Property
  -> MiniLight Component

-- | Generate an unique id.
newUID :: MonadIO m => m T.Text
newUID = liftIO $ Data.UUID.toText <$> Data.UUID.V4.nextRandom

-- | Create a component with given resolver.
createComponentBy :: Resolver -> ComponentConfig -> MiniLight Component
createComponentBy resolver config = resolver (name config) (properties config)
