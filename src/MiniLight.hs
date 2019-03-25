module MiniLight where

import Control.Monad.Reader
import Lens.Micro
import qualified SDL as SDL

data LightEnv = LightEnv {
  renderer :: SDL.Renderer
}

class HasLightEnv env where
  rendererL :: Lens' env SDL.Renderer

instance HasLightEnv LightEnv where
  rendererL = lens renderer (\env r -> env { renderer = r })

newtype LightT env m a = LightT { runLightT :: ReaderT env m a }
type MiniLight = LightT LightEnv IO



