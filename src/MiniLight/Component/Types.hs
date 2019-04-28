{-# LANGUAGE ExistentialQuantification #-}
module MiniLight.Component.Types (
  Resolver,
  HasComponentEnv(..),
  emit,

  ComponentUnit(..),
  Component,
  newComponent,
  getComponentSize,
  getUID,
  newUID,
  propagate,
) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.IORef
import qualified Data.UUID
import qualified Data.UUID.V4
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Mtl
import MiniLight.Light
import MiniLight.Event
import MiniLight.Figure
import qualified SDL

-- | The type for component resolver
type Resolver
  = T.Text  -- ^ Component Type
  -> T.Text  -- ^ UID
  -> Value  -- ^ Component Property
  -> MiniLight Component

class HasComponentEnv env where
  -- | Lens to the unique id, which is provided for each component.
  uidL :: Lens' env T.Text

-- | Emit a signal, which will be catched at the next frame.
emit
  :: (HasLoopEnv env, HasComponentEnv env, MonadIO m, EventType et)
  => et
  -> LightT env m ()
emit et = do
  uid <- view uidL
  ref <- view signalQueueL
  liftIO $ modifyIORef' ref $ (signal uid et :)

-- | CompoonentUnit typeclass provides a way to define a new component.
-- Any 'ComponentUnit' instance can be embedded into 'Component' type.
class ComponentUnit c where
  -- | Updating a model.
  update :: (HasLightEnv env, HasLoopEnv env, HasComponentEnv env, MonadIO m, MonadMask m) => c -> LightT env m c
  update = return

  -- | Descirbes a view. The figures here would be cached. See also 'useCache' for the cache configuration.
  figures :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m [Figure]

  -- | Drawing a figures.
  draw :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m ()
  draw comp = liftMiniLight . renders =<< figures comp
  {-# INLINE draw #-}

  -- | Event handlers
  onSignal :: (HasLightEnv env, HasLoopEnv env, HasComponentEnv env, MonadIO m, MonadMask m) => Event -> c -> LightT env m c
  onSignal _ = return

  -- | Return @True@ if a cache stored in the previous frame should be used.
  useCache
    :: c  -- ^ A model value in the previous frame
    -> c  -- ^ A model value in the current frame
    -> Bool
  useCache _ _ = False

  -- | To be called just before clearing caches.
  -- If you want to destroy cached textures for memory efficiency, override this method.
  --
  -- __NB__: Freeing SDL textures and figures are not performed automatically. You must call 'freeFigure' at your own risk.
  beforeClearCache :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> [Figure] -> LightT env m ()
  beforeClearCache _ _ = return ()

-- | A wrapper for 'ComponentUnit' instances.
data Component = forall c. ComponentUnit c => Component {
  uid :: T.Text,
  component :: c,
  prev :: c,
  cache :: IORef [Figure]
}

-- | Create a new component.
newComponent
  :: (ComponentUnit c, HasLightEnv env, MonadIO m, MonadMask m)
  => T.Text
  -> c
  -> LightT env m Component
newComponent uid c = do
  figs <- figures c
  ref  <- liftIO $ newIORef figs
  return $ Component {uid = uid, component = c, prev = c, cache = ref}

-- | Get the size of a component.
getComponentSize
  :: (ComponentUnit c, HasLightEnv env, MonadIO m, MonadMask m)
  => c
  -> LightT env m (SDL.Rectangle Int)
getComponentSize comp = do
  figs <- figures comp
  return $ foldl union (SDL.Rectangle (SDL.P 0) 0) $ map targetArea figs

-- | Get its unique id.
getUID :: Component -> T.Text
getUID (Component uid _ _ _) = uid

-- | Generate an unique id.
newUID :: MonadIO m => m T.Text
newUID = liftIO $ Data.UUID.toText <$> Data.UUID.V4.nextRandom

-- | Clear the previous model cache and reflect the current model.
propagate :: Component -> Component
propagate (Component uid comp _ cache) = Component uid comp comp cache

instance ComponentUnit Component where
  update (Component uid comp prev cache) = do
    comp' <- update comp
    return $ Component uid comp' prev cache

  figures (Component _ comp _ _) = figures comp

  draw (Component _ comp prev ref) = do
    if useCache prev comp
      then liftMiniLight . renders =<< liftIO (readIORef ref)
      else do
        figs <- liftIO (readIORef ref)
        beforeClearCache comp figs

        figs <- figures comp
        liftMiniLight $ renders figs
        liftIO $ writeIORef ref figs

  onSignal ev (Component uid comp prev cache) = fmap (\comp' -> Component uid comp' prev cache) $ onSignal ev comp
