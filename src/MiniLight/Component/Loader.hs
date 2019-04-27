{-# LANGUAGE DeriveGeneric #-}
module MiniLight.Component.Loader where

import Control.Monad.IO.Class
import Data.Aeson hiding (Result)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
import GHC.Generics
import MiniLight.Light
import MiniLight.Component.Types
import MiniLight.Component.Internal.Resolver (resolve)

data ComponentConfig = ComponentConfig {
  name :: T.Text,
  properties :: Value
} deriving Generic

instance FromJSON ComponentConfig

data AppConfig = AppConfig {
  app :: [ComponentConfig]
} deriving Generic

instance FromJSON AppConfig

-- | Load an config file and construct components.
loadAppConfig
  :: (HasLightEnv env, MonadIO m)
  => FilePath  -- ^ Filepath to the yaml file.
  -> (T.Text -> Value -> LightT env m Component)  -- ^ Specify any resolver.
  -> LightT env m [Component]
loadAppConfig path mapper = do
  conf <-
    liftIO
    $   (\(Data.Aeson.Success a) -> a)
    .   fromJSON
    .   resolve
    .   either (error . show) id
    <$> decodeFileEither path
  mapM (\conf -> mapper (name conf) (properties conf)) (app conf)
