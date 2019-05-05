{-# LANGUAGE DeriveGeneric #-}
module MiniLight.Loader.Internal.Types where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.UUID
import qualified Data.UUID.V4
import GHC.Generics
import MiniLight.Component
import MiniLight.Light

data ComponentConfig = ComponentConfig {
  name :: T.Text,
  properties :: Value,
  hooks :: Maybe Object
} deriving (Eq, Show, Generic)

instance ToJSON ComponentConfig
instance FromJSON ComponentConfig

data AppConfig = AppConfig {
  app :: V.Vector ComponentConfig,
  uuid :: V.Vector T.Text
} deriving (Eq, Show)

instance FromJSON AppConfig where
  parseJSON = withObject "app" $ \v -> do
    app <- v .: "app"

    return $ AppConfig app V.empty

-- | The type for component resolver
type Resolver
  = T.Text  -- ^ Component Type
  -> T.Text  -- ^ UID
  -> Value  -- ^ Component Property
  -> MiniLight (Either String Component)

-- | Generate an unique id.
newUID :: MonadIO m => m T.Text
newUID = liftIO $ Data.UUID.toText <$> Data.UUID.V4.nextRandom

-- | Create a component with given resolver.
createComponentBy
  :: Resolver
  -> Maybe T.Text
  -> ComponentConfig
  -> MiniLight (Either String Component)
createComponentBy resolver uid config =
  maybe newUID return uid >>= \u -> resolver (name config) u (properties config)
