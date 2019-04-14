{-# LANGUAGE ExistentialQuantification #-}
module MiniLight.Component.Types (
  ComponentUnit(..),
  Component,
  newComponent,
  getComponentSize,
  propagate,
) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import MiniLight.Light
import MiniLight.Event
import MiniLight.Figure
import qualified SDL

-- | CompoonentUnit typeclass provides a way to define a new component.
-- Any 'ComponentUnit' instance can be embedded into 'Component' type.
class ComponentUnit c where
  -- | Updating a model.
  update :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m c
  update = return

  -- | Descirbes a view. The figures here would be cached. See also 'useCache' for the cache configuration.
  figures :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m [Figure]

  -- | Drawing a figures.
  draw :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m ()
  draw comp = liftMiniLight . renders =<< figures comp
  {-# INLINE draw #-}

  -- | Event handlers
  onSignal :: (HasLightEnv env, MonadIO m, MonadMask m) => Event -> c -> LightT env m c
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
  component :: c,
  prev :: c,
  cache :: IORef [Figure]
}

-- | Create a new component.
newComponent
  :: (ComponentUnit c, HasLightEnv env, MonadIO m, MonadMask m)
  => c
  -> LightT env m Component
newComponent c = do
  figs <- figures c
  ref  <- liftIO $ newIORef figs
  return $ Component {component = c, prev = c, cache = ref}

-- | Get the size of a component.
getComponentSize
  :: (ComponentUnit c, HasLightEnv env, MonadIO m, MonadMask m)
  => c
  -> LightT env m (SDL.Rectangle Int)
getComponentSize comp = do
  figs <- figures comp
  return $ foldl union (SDL.Rectangle (SDL.P 0) 0) $ map targetArea figs

-- | Clear the previous model cache and reflect the current model.
propagate :: Component -> Component
propagate (Component comp _ cache) = Component comp comp cache

instance ComponentUnit Component where
  update (Component comp prev cache) = do
    comp' <- update comp
    return $ Component comp' prev cache

  figures (Component comp _ _) = figures comp

  draw (Component comp prev ref) = liftMiniLight $ do
    if useCache prev comp
      then renders =<< liftIO (readIORef ref)
      else do
        figs <- liftIO (readIORef ref)
        beforeClearCache comp figs

        figs <- figures comp
        renders figs
        liftIO $ writeIORef ref figs

  onSignal ev (Component comp prev cache) = fmap (\comp' -> Component comp' prev cache) $ onSignal ev comp
