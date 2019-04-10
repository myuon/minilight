{-# LANGUAGE ExistentialQuantification #-}
module MiniLight.Component.Types (
  ComponentUnit(..),
  Component,
  newComponent,
  getComponentSize,
) where

import Control.Monad.Catch
import Control.Monad.IO.Class
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

  useCache :: c -> Bool
  useCache _ = False

  onSignal :: (HasLightEnv env, MonadIO m, MonadMask m) => Event -> c -> LightT env m c
  onSignal _ = return

data Component = forall c. ComponentUnit c => Component {
  component :: c,
  cache :: [Figure]
}

newComponent
  :: (ComponentUnit c, HasLightEnv env, MonadIO m, MonadMask m)
  => c
  -> LightT env m Component
newComponent c = do
  figs <- figures c
  return $ Component {component = c, cache = figs}

getComponentSize
  :: (ComponentUnit c, HasLightEnv env, MonadIO m, MonadMask m)
  => c
  -> LightT env m (SDL.Rectangle Int)
getComponentSize comp = do
  figs <- figures comp
  return $ foldl union (SDL.Rectangle (SDL.P 0) 0) $ map targetArea figs

instance ComponentUnit Component where
  update (Component comp cache) = do
    comp' <- update comp
    return $ Component comp' cache

  figures (Component comp _) = figures comp

  draw (Component comp cache) = liftMiniLight $ do
    if useCache comp
      then renders cache
      else renders =<< figures comp

  onSignal ev (Component comp cache) = fmap (\comp' -> Component comp' cache) $ onSignal ev comp
