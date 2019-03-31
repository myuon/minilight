{-# LANGUAGE ExistentialQuantification #-}
module MiniLight.Component.Types where

import Control.Monad.IO.Class
import MiniLight.Light

class ComponentUnit c where
  update :: (HasLightEnv env, MonadIO m) => c -> LightT env m c
  draw :: (HasLightEnv env, MonadIO m) => c -> LightT env m ()

data Component = forall c. ComponentUnit c => Component { getComponent :: c }

instance ComponentUnit Component where
  update comp = case comp of Component c -> fmap Component $ update c
  draw comp = case comp of Component c -> draw c

