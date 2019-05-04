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
import qualified Control.Monad.Caster as Caster
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Aeson.Diff as Diff
import Data.Aeson.Patch
import Data.Aeson.Pointer
import Data.IORef
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
    $   (toEither . fromJSON <=< fmap resolve)
    .   either (Left . show) Right
    <$> decodeFileEither path

-- | Load an config file and set in the environment. Calling this function at once, this overrides all values in the environment.
-- This will generate an error log and skip the component if the configuration is invalid.
-- This function also assign unique IDs for each component, using 'assignUID'.
loadAppConfig
  :: (HasLightEnv env, HasLoaderEnv env, MonadIO m, MonadCatch m)
  => FilePath  -- ^ Filepath to the yaml file.
  -> Resolver  -- ^ Specify any resolver.
  -> LightT env m ()
loadAppConfig path mapper = fmap (maybe () id) $ runMaybeT $ do
  conf <- resolveConfig path >>= \case
    Left e -> do
      lift $ Caster.err e
      fail ""
    Right r -> return r

  confs <- lift $ fmap (V.mapMaybe id) $ V.forM (app conf) $ \conf -> do
    uid    <- newUID
    result <- liftMiniLight $ mapper (name conf) uid (properties conf)

    case result of
      Left e -> do
        Caster.err e
        return Nothing
      Right component -> do
        register component

        Caster.info
          $  "Component loaded: {name: "
          <> show (name conf)
          <> ", uid: "
          <> show uid
          <> "}"

        return $ Just (uid, conf)

  ref <- view _appConfig
  liftIO $ writeIORef ref $ AppConfig (V.map snd confs) (V.map fst confs)

-- | Load the config file again and place the newly loaded components. This is used for HCR (hot component replacement).
-- Call 'loadAppConfig' first.
patchAppConfig
  :: (HasLightEnv env, HasLoaderEnv env, MonadIO m, MonadCatch m)
  => FilePath  -- ^ Filepath to the yaml file.
  -> Resolver  -- ^ Specify any resolver.
  -> LightT env m ()
patchAppConfig path resolver = fmap (maybe () id) $ runMaybeT $ do
  cref      <- view _appConfig
  appConfig <- liftIO $ readIORef cref

  conf'     <- do
    mconf <- lift $ resolveConfig path

    case mconf of
      Left e -> do
        lift $ Caster.err e
        fail ""
      Right r -> return r

  lift
    $ forM_
        ( Diff.patchOperations
        $ Diff.diff (toJSON $ app appConfig) (toJSON $ app conf')
        )
    $ \op -> fmap (maybe () id) $ runMaybeT $ do
        lift $ Caster.debug $ "CMR detected: " <> show op

        case op of
          Add (Pointer [AKey _]) v      -> create v
          Rem (Pointer [AKey n])        -> remove n
          Rep (Pointer (AKey n:path)) v -> modify n path v
          _                             -> return ()
 where
  create v = do
    cref     <- view _appConfig
    compConf <- case fromJSON v of
      Success a   -> return a
      Error   err -> do
        lift $ Caster.err err
        fail ""

    newID     <- lift newUID
    component <- do
      result <- lift $ liftMiniLight $ createComponentBy resolver
                                                         (Just newID)
                                                         compConf

      case result of
        Left err -> do
          lift $ Caster.err $ "Failed to resolve: " <> err
          fail ""
        Right c -> return c

    lift $ register component
    lift
      $  Caster.info
      $  "Component registered: {name: "
      <> show (name compConf)
      <> ", uid: "
      <> show (getUID component)
      <> "}"

    liftIO $ modifyIORef' cref $ \conf -> conf
      { app  = V.snoc (app conf) compConf
      , uuid = V.snoc (uuid conf) newID
      }

  remove n = do
    cref    <- view _appConfig
    appConf <- liftIO $ readIORef cref

    let uid = uuid appConf V.! n

    reg <- view _registry
    lift $ R.delete reg uid
    lift $ Caster.info $ "Component deleted: {uid: " <> show uid <> "}"

    liftIO $ writeIORef cref $ appConf
      { app  = V.ifilter (\i _ -> i /= n) $ app appConf
      , uuid = V.ifilter (\i _ -> i /= n) $ uuid appConf
      }

  modify n path v = do
    cref     <- view _appConfig
    appConf  <- liftIO $ readIORef cref

    compConf <-
      case
        Diff.applyOperation (Rep (Pointer path) v) (toJSON (app appConf V.! n))
          >>= fromJSON
      of
        Success a   -> return a
        Error   err -> do
          lift $ Caster.err err
          fail ""

    let uid = uuid appConf V.! n

    component <- do
      result <- lift $ liftMiniLight $ createComponentBy resolver
                                                         (Just uid)
                                                         compConf

      case result of
        Left err -> do
          lift $ Caster.err $ "Failed to resolve: " <> err
          fail ""
        Right c -> return c

    reg <- view _registry
    lift $ R.write reg uid component
    lift
      $  Caster.info
      $  "Component replaced: {name: "
      <> show (name compConf)
      <> ", uid: "
      <> show (getUID component)
      <> "}"

    liftIO $ writeIORef cref $ appConf { app = app appConf V.// [(n, compConf)]
                                       }

-- | Register a component to the component registry.
register :: (HasLoaderEnv env, MonadIO m) => Component -> LightT env m ()
register component = do
  reg <- view _registry
  R.register reg (getUID component) component
