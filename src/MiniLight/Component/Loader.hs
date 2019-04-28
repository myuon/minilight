module MiniLight.Component.Loader (
  module MiniLight.Component.Internal.Types,
  module MiniLight.Component.Internal.Diff,

  decodeAndResolveConfig,
  loadAppConfig,
) where

import Control.Monad.IO.Class
import Data.Aeson hiding (Result)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
import MiniLight.Light
import MiniLight.Component.Types
import MiniLight.Component.Internal.Types
import MiniLight.Component.Internal.Diff
import MiniLight.Component.Internal.Resolver (resolve)

-- | Load an config file and return the resolved @AppConfig@.
decodeAndResolveConfig :: MonadIO m => FilePath -> m AppConfig
decodeAndResolveConfig path =
  liftIO
    $   (\(Data.Aeson.Success a) -> a)
    .   fromJSON
    .   resolve
    .   either (error . show) id
    <$> decodeFileEither path

-- | Load an config file and return with the constructed components.
loadAppConfig
  :: (HasLightEnv env, MonadIO m)
  => FilePath  -- ^ Filepath to the yaml file.
  -> (T.Text -> Value -> LightT env m Component)  -- ^ Specify any resolver.
  -> LightT env m (AppConfig, [Component])
loadAppConfig path mapper = do
  conf <- decodeAndResolveConfig path
  (,) <$> pure conf <*> mapM (\conf -> mapper (name conf) (properties conf))
                             (app conf)

-- | A variant of 'loadAppConfig', if you don't need @AppConfig@.
loadAppConfig_
  :: (HasLightEnv env, MonadIO m)
  => FilePath  -- ^ Filepath to the yaml file.
  -> (T.Text -> Value -> LightT env m Component)  -- ^ Specify any resolver.
  -> LightT env m [Component]
loadAppConfig_ path mapper = fmap snd $ loadAppConfig path mapper
