module MiniLight.Component.Loader (
  module MiniLight.Component.Internal.Types,
  module MiniLight.Component.Internal.Diff,

  LoaderEnv (..),
  HasLoaderEnv (..),

  resolveConfig,
  resolveAndAssignUIDConfig,
  loadAppConfig,
  patchAppConfig,
  register,
  assignUID,
) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import qualified Control.Monad.Caster as Caster
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Aeson.Diff as Diff
import Data.Aeson.Patch
import Data.Aeson.Pointer
import Data.IORef
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Registry as R
import qualified Data.Vector as V
import Data.Yaml (decodeFileEither)
import MiniLight.Light
import MiniLight.Component.Types
import MiniLight.Component.Internal.Types
import MiniLight.Component.Internal.Diff
import MiniLight.Component.Internal.Resolver (resolve)

-- | The environment for config loader
data LoaderEnv = LoaderEnv {
  registry :: R.Registry Component,
  appConfig :: IORef AppConfig
}

makeClassy_ ''LoaderEnv

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

-- | Load an config file and set in the environment. Calling this function at once, this overrides all values in the environment.
-- This will generate an error log and skip the component if the configuration is invalid.
-- This function also assign unique IDs for each component, using 'assignUID'.
loadAppConfig
  :: (HasLightEnv env, HasLoaderEnv env, MonadIO m, MonadCatch m)
  => FilePath  -- ^ Filepath to the yaml file.
  -> Resolver  -- ^ Specify any resolver.
  -> LightT env m ()
loadAppConfig path mapper = do
  conf <- resolveAndAssignUIDConfig path

  (\m -> either Caster.err m conf) $ \conf -> do
    confs <- fmap (V.mapMaybe id) $ V.forM (app conf) $ \conf -> do
      result <- liftMiniLight
        $ mapper (name conf) (fromJust $ uid conf) (properties conf)

      flip (either (\err -> Caster.err err >> return Nothing)) result
        $ (fmap Just .)
        $ \component -> do
            register component

            Caster.info
              $  "Component loaded: {name: "
              <> show (name conf)
              <> ", uid = "
              <> show (uid conf)
              <> "}"

            return conf

    ref <- view _appConfig
    liftIO $ writeIORef ref $ AppConfig confs

-- | Load the config file again and place the newly loaded components. This is used for HCR (hot component replacement).
-- Call 'loadAppConfig' first.
patchAppConfig
  :: (HasLightEnv env, HasLoaderEnv env, MonadIO m, MonadCatch m)
  => FilePath  -- ^ Filepath to the yaml file.
  -> Resolver  -- ^ Specify any resolver.
  -> LightT env m ()
patchAppConfig path resolver = do
  cref  <- view _appConfig
  conf  <- liftIO $ readIORef cref
  conf' <- resolveConfig path

  (liftIO . writeIORef cref =<<)
    $ fmap
        ( (\(Success a) -> a)
        . fromJSON
        . (\y -> (\(Success a) -> a) $ Diff.patch (Patch y) (toJSON conf))
        . catMaybes
        )
    $ forM (Diff.patchOperations $ Diff.diff (toJSON conf) (toJSON conf'))
    $ \op -> do
        Caster.info $ "CMR detected: " <> show op

        case op of
          Add path@(Pointer [OKey "app", AKey i]) v -> do
            let conf = (\(Success a) -> a) $ fromJSON v

            newID  <- newUID
            result <- liftMiniLight
              $ resolver (name conf) newID (properties conf)

            either
              ( \e -> do
                Caster.err $ "Component creation failed: " <> e
                return Nothing
              )
              ( \c -> do
                register c
                Caster.info
                  $  "Component inserted: {name: "
                  <> show (name conf)
                  <> ", uid = "
                  <> show (uid conf)
                  <> "}"
                return $ Just op
              )
              result
          _ -> return Nothing

register :: (HasLoaderEnv env, MonadIO m) => Component -> LightT env m ()
register component = do
  reg <- view _registry
  R.register reg (getUID component) component

-- | Assign unique IDs to each component configuration.
assignUID :: MonadIO m => AppConfig -> m AppConfig
assignUID (AppConfig cs) = liftIO $ fmap AppConfig $ mapM
  ( \conf ->
    maybe (newUID >>= \uid -> return (conf { uid = Just uid }))
          (\_ -> return conf)
      $ uid conf
  )
  cs
