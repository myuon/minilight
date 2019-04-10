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

class ComponentUnit c where
  update :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m c
  update = return

  figures :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m [Figure]

  draw :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m ()
  draw comp = liftMiniLight . renders =<< figures comp
  {-# INLINE draw #-}

  useCache :: c -> c -> Bool
  useCache _ _ = False

  onSignal :: (HasLightEnv env, MonadIO m, MonadMask m) => Event -> c -> LightT env m c
  onSignal _ = return

data Component = forall c. ComponentUnit c => Component {
  component :: c,
  prev :: c,
  cache :: IORef [Figure]
}

newComponent
  :: (ComponentUnit c, HasLightEnv env, MonadIO m, MonadMask m)
  => c
  -> LightT env m Component
newComponent c = do
  figs <- figures c
  ref  <- liftIO $ newIORef figs
  return $ Component {component = c, prev = c, cache = ref}

getComponentSize
  :: (ComponentUnit c, HasLightEnv env, MonadIO m, MonadMask m)
  => c
  -> LightT env m (SDL.Rectangle Int)
getComponentSize comp = do
  figs <- figures comp
  return $ foldl union (SDL.Rectangle (SDL.P 0) 0) $ map targetArea figs

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
        mapM_ freeFigure figs

        figs <- figures comp
        renders figs
        liftIO $ writeIORef ref figs

  onSignal ev (Component comp prev cache) = fmap (\comp' -> Component comp' prev cache) $ onSignal ev comp
