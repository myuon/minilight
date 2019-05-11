{-| The package provides the configuration loader.

An configuration example:

@
_vars:
  window:
    width: 800
    height: 600
app:
  - name: message-layer
    properties:
      window:
        image: resources/window-base.png
        position:
          x: 0
          y: ${${var:window.height} - ${ref:..size.height}}
        size:
          width: ${var:window.width}
          height: 150
@

== Syntax

=== @_vars@

You can define a new variable. Use object syntax under the @_vars@ field.

The variables can be referenced in all siblings and under their siblings to the @_vars@, in the variable syntax @${var:_path_}@.

=== Expr

In each field, you can specify an expression defined in the loader.

- @${}@: enclose the expr by @${}@, to tell the parsers that the field is an expr not a plain string.
- @${ref:_path_}@: specify any path to refer any other value. The path resolution is performed once, not recursively resolved. @_path_@ consists of field names splitted by a period. Use double dots @..@ for a parent.
- @${var:_path_}@: specify any path to value defined at the field. @_path_@ consists of field names splitted by a period.
- arithmetic operator: addition, subtraction, multiplication and division (@+,-,*,/@) can also be used in @${}@.

-}
module MiniLight.Loader (
  module MiniLight.Loader.Internal.Types,

  LoaderEnv (..),
  HasLoaderEnv (..),

  resolveConfig,
  loadAppConfig,
  patchAppConfig,

  resolve,
  parseAppConfig,
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
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Yaml (decodeFileEither)
import MiniLight.Light
import MiniLight.Component
import MiniLight.Loader.Internal.Types
import MiniLight.Loader.Internal.Resolver (resolve, parseAppConfig)

-- | The environment for config loader
data LoaderEnv = LoaderEnv {
  registry :: R.Registry Component,
  appConfig :: IORef AppConfig
}

makeClassy_ ''LoaderEnv

-- | Load an config file and return the resolved @AppConfig@.
resolveConfig :: MonadIO m => FilePath -> m (Either T.Text AppConfig)
resolveConfig path =
  liftIO
    $   (parseAppConfig <=< either (Left . T.pack . show) Right)
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
        reg <- view _registry
        R.register reg uid component

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
          Add (Pointer [AKey n]) v      -> create n v
          Rem (Pointer [AKey n])        -> remove n
          Rep (Pointer [AKey n     ]) v -> modify n (Rep (Pointer []) v)
          Rep (Pointer (AKey n:path)) v -> modify n (Rep (Pointer path) v)
          Add (Pointer (AKey n:path)) v -> modify n (Add (Pointer path) v)
          Rem (Pointer (AKey n:path))   -> modify n (Rem (Pointer path))
          _ ->
            lift
              $  Caster.warn
              $  "CMR does not support the operation yet: "
              <> show op
 where
  create n v = do
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

    reg <- view _registry
    lift $ R.insert reg n (getUID component) component

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

  modify n op = do
    cref     <- view _appConfig
    appConf  <- liftIO $ readIORef cref

    compConf <-
      case Diff.applyOperation op (toJSON (app appConf V.! n)) >>= fromJSON of
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
