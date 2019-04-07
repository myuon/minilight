module MiniLight.Component.AnimationLayer where

import Control.Monad.State
import Data.Aeson
import Linear
import Lens.Micro
import MiniLight.Figure
import MiniLight.Light
import qualified MiniLight.Component.Layer as Layer
import MiniLight.Component.Types
import qualified SDL
import qualified SDL.Vect as Vect

data AnimationLayer = AnimationLayer {
  layer :: Layer.Layer,
  counter :: Int,
  textureSize :: Vect.V2 Int,
  scaler :: Int,
  config :: Config
}

instance ComponentUnit AnimationLayer where
  update = execStateT $ do
    modify $ \c -> c { counter = (counter c + 1) }
    modify $ \c -> c { counter = if counter c >= (division (config c) ^._x * division (config c) ^. _y) * scaler c then 0 else counter c }

  figures comp = do
    let iv = V2 ((counter comp `div` scaler comp) `mod` division (config comp) ^. _x) ((counter comp `div` scaler comp) `div` division (config comp) ^. _x)
    let tileSize = div <$> textureSize comp <*> division (config comp)
    return [
      clip (SDL.Rectangle (SDL.P (tileSize * iv)) tileSize) $ Layer.layer $ layer comp
      ]

data Config = Config {
  layerConf :: Layer.Config,
  division :: Vect.V2 Int
}

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    conf <- parseJSON (Object v)
    division <- (\v -> Vect.V2 <$> v .: "x" <*> v .: "y") =<< v .: "division"

    return $ Config conf division

new :: Config -> MiniLight AnimationLayer
new conf = do
  layer <- Layer.new (layerConf conf)
  size  <- getFigureSize (Layer.layer layer)

  return $ AnimationLayer
    { layer       = layer
    , counter     = 0
    , textureSize = fmap fromEnum size
    , scaler      = 25
    , config      = conf
    }

