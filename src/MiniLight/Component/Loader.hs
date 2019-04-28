module MiniLight.Component.Loader (
  module MiniLight.Component.Internal.Types,
  module MiniLight.Component.Internal.Diff,

  resolveConfig,
  resolveAndAssignUIDConfig,
  loadAppConfig,
  loadAppConfig_,
  assignUID,
) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Yaml (decodeFileEither)
import MiniLight.Light
import MiniLight.Component.Types
import MiniLight.Component.Internal.Types
import MiniLight.Component.Internal.Diff
import MiniLight.Component.Internal.Resolver (resolve)

toEither :: Result a -> Either String a
toEither (Error   s) = Left s
toEither (Success a) = Right a

-- | Load an config file and return the resolved @AppConfig@.
resolveConfig :: MonadIO m => FilePath -> m (Either String AppConfig)
resolveConfig path =
  liftIO
    $   toEither
    .   fromJSON
    .   resolve
    .   either (error . show) id
    <$> decodeFileEither path

-- | Perform 'resolveConfig' and 'assignUID'.
-- @
-- resolveAndAssignUIDConfig path = resolveConfig path >>= sequence . fmap assignUID
-- @
resolveAndAssignUIDConfig
  :: MonadIO m => FilePath -> m (Either String AppConfig)
resolveAndAssignUIDConfig path =
  resolveConfig path >>= sequence . fmap assignUID

-- | Load an config file and return it with the constructed components.
-- This will cause a runtime exception if the config file cannot be parsed.
-- This function also assign unique IDs for each component, using 'assignUID'.
loadAppConfig
  :: (HasLightEnv env, MonadIO m)
  => FilePath  -- ^ Filepath to the yaml file.
  -> Resolver  -- ^ Specify any resolver.
  -> LightT env m (AppConfig, [Component])
loadAppConfig path mapper = do
  resolveAndAssignUIDConfig path >>= \case
    Left  err  -> error err
    Right conf -> (,) <$> pure conf <*> mapM
      ( \conf -> liftMiniLight
        $ mapper (name conf) (fromJust $ uid conf) (properties conf)
      )
      (app conf)

-- | A variant of 'loadAppConfig', if you don't need @AppConfig@.
loadAppConfig_
  :: (HasLightEnv env, MonadIO m)
  => FilePath  -- ^ Filepath to the yaml file.
  -> Resolver  -- ^ Specify any resolver.
  -> LightT env m [Component]
loadAppConfig_ path mapper = fmap snd $ loadAppConfig path mapper

-- | Assign unique IDs to each component configuration.
assignUID :: MonadIO m => AppConfig -> m AppConfig
assignUID (AppConfig cs) = liftIO $ fmap AppConfig $ mapM
  ( \conf ->
    maybe (newUID >>= \uid -> return (conf { uid = Just uid }))
          (\_ -> return conf)
      $ uid conf
  )
  cs
