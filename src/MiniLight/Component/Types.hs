{-# LANGUAGE ExistentialQuantification #-}
module MiniLight.Component.Types where

import Control.Monad.Catch
import Control.Monad.IO.Class
import MiniLight.Light
import MiniLight.Figure
import qualified SDL

class ComponentUnit c where
  update :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m c
  update = return

  figures :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m [Figure]

  draw :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m ()
  draw comp = liftMiniLight . renders =<< figures comp
  {-# INLINE draw #-}

data Component = forall c. ComponentUnit c => Component { getComponent :: c }

getComponentSize
  :: (ComponentUnit c, HasLightEnv env, MonadIO m, MonadMask m)
  => c
  -> LightT env m (SDL.Rectangle Int)
getComponentSize comp = do
  figs <- figures comp
  return $ foldl union (SDL.Rectangle (SDL.P 0) 0) $ map targetArea figs

instance ComponentUnit Component where
  update comp = case comp of Component c -> fmap Component $ update c
  figures comp = case comp of Component c -> figures c

