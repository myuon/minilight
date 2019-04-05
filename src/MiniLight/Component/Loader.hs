{-# LANGUAGE DeriveGeneric #-}
module MiniLight.Component.Loader (
  loadAppConfig
) where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
import GHC.Generics
import MiniLight.Light
import MiniLight.Component.Types

data ComponentConfig = ComponentConfig {
  name :: T.Text,
  properties :: Value
} deriving Generic

instance FromJSON ComponentConfig

data AppConfig = AppConfig {
  app :: [ComponentConfig]
} deriving Generic

instance FromJSON AppConfig

loadAppConfig
  :: FilePath
  -> (T.Text -> Value -> MiniLight Component)
  -> MiniLight [Component]
loadAppConfig path mapper = do
  conf <- liftIO $ either (error . show) id <$> decodeFileEither path
  mapM (\conf -> mapper (name conf) (properties conf)) (app conf)

