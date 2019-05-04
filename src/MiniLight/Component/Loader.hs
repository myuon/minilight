module MiniLight.Component.Loader (
  module MiniLight.Component.Internal.Types,

  LoaderEnv (..),
  HasLoaderEnv (..),

  resolveConfig,
  loadAppConfig,
  patchAppConfig,
  register,
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
import Data.Maybe (catMaybes)
import qualified Data.Registry as R
import qualified Data.Vector as V
import Data.Yaml (decodeFileEither)
import MiniLight.Light
import MiniLight.Component.Types
import MiniLight.Component.Internal.Types
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

-- | Load an config file and set in the environment. Calling this function at once, this overrides all values in the environment.
-- This will generate an error log and skip the component if the configuration is invalid.
-- This function also assign unique IDs for each component, using 'assignUID'.
loadAppConfig
  :: (HasLightEnv env, HasLoaderEnv env, MonadIO m, MonadCatch m)
  => FilePath  -- ^ Filepath to the yaml file.
  -> Resolver  -- ^ Specify any resolver.
  -> LightT env m ()
loadAppConfig path mapper = do
  conf <- resolveConfig path

  (\m -> either Caster.err m conf) $ \conf -> do
    confs <- fmap (V.mapMaybe id) $ V.forM (app conf) $ \conf -> do
      uid    <- newUID
      result <- liftMiniLight $ mapper (name conf) uid (properties conf)

      flip (either (\err -> Caster.err err >> return Nothing)) result
        $ (fmap Just .)
        $ \component -> do
            register component

            Caster.info
              $  "Component loaded: {name: "
              <> show (name conf)
              <> ", uid = "
              <> show uid
              <> "}"

            return (uid, conf)

    ref <- view _appConfig
    liftIO $ writeIORef ref $ AppConfig (V.map snd confs) (V.map fst confs)

-- | Load the config file again and place the newly loaded components. This is used for HCR (hot component replacement).
-- Call 'loadAppConfig' first.
patchAppConfig
  :: (HasLightEnv env, HasLoaderEnv env, MonadIO m, MonadCatch m)
  => FilePath  -- ^ Filepath to the yaml file.
  -> Resolver  -- ^ Specify any resolver.
  -> LightT env m ()
patchAppConfig path resolver = do
  cref      <- view _appConfig
  appConfig <- liftIO $ readIORef cref

  compsV    <- liftIO $ V.unsafeThaw (app appConfig)
  uuidsV    <- liftIO $ V.unsafeThaw (uuid appConfig)

  mconf'    <- resolveConfig path

  case mconf' of
    Left e -> Caster.err e
    Right conf' ->
      forM_
          ( Diff.patchOperations
          $ Diff.diff (toJSON $ app appConfig) (toJSON $ app conf')
          )
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

-- | Register a component to the component registry.
register :: (HasLoaderEnv env, MonadIO m) => Component -> LightT env m ()
register component = do
  reg <- view _registry
  R.register reg (getUID component) component
