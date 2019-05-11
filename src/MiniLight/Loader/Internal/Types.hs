{-# LANGUAGE DeriveGeneric #-}
module MiniLight.Loader.Internal.Types where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.UUID
import qualified Data.UUID.V4
import MiniLight.Component
import MiniLight.Light

newtype Hook = Hook { runHook :: Value -> MiniLight () }

-- | A configuration for a component
data ComponentConfig = ComponentConfig {
  name :: T.Text,
  properties :: Value,
  hooks :: Maybe (HM.HashMap String Hook)
}

instance ToJSON ComponentConfig where
  toJSON v = toJSON $ HM.fromList [("name" :: String, String (name v)), ("properties", properties v)]

instance FromJSON ComponentConfig where
  parseJSON = withObject "component" $ \v -> do
    name <- v .: "name"
    props <- v .: "properties"

    return $ ComponentConfig name props Nothing

-- | A configuration for the application itself
data AppConfig = AppConfig {
  app :: V.Vector ComponentConfig,
  uuid :: V.Vector T.Text
}

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