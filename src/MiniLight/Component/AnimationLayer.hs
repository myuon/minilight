module MiniLight.Component.AnimationLayer where

import Control.Monad.State
import Foreign.C.Types (CInt)
import Linear
import Lens.Micro
import Lens.Micro.Mtl
import MiniLight.Figure
import MiniLight.Light
import qualified MiniLight.Component.Layer as L
import MiniLight.Component.Types
import qualified SDL
import qualified SDL.Vect as Vect

data AnimationLayer = AnimationLayer {
  layer :: L.Layer,
  counter :: Int,
  textureSize :: Vect.V2 Int,
  division :: Vect.V2 Int,
  scaler :: Int
}

instance ComponentUnit AnimationLayer where
  update = execStateT $ do
    modify $ \c -> c { counter = (counter c + 1) }
    modify $ \c -> c { counter = if counter c >= (division c ^._x * division c ^. _y) * scaler c then 0 else counter c }

  draw comp = do
    let iv = V2 ((counter comp `div` scaler comp) `mod` division comp ^. _x) ((counter comp `div` scaler comp) `div` division comp ^. _x)
    let tileSize = div <$> textureSize comp <*> division comp

    liftMiniLight $ render $ clip (SDL.Rectangle (SDL.P (tileSize * iv)) tileSize) $ L.layer $ layer comp

new :: FilePath -> Vect.V2 Int -> MiniLight AnimationLayer
new path division = do
  layer <- L.new path
  size  <- getTextureSize (L.layer layer)

  return $ AnimationLayer
    { layer       = layer
    , counter     = 0
    , textureSize = fmap fromEnum size
    , division    = division
    , scaler      = 25
    }

