module Data.Component.AnimationLayer where

import Control.Monad.State
import Data.Aeson
import Linear
import Lens.Micro
import MiniLight
import qualified Data.Component.Layer as Layer
import qualified SDL
import qualified SDL.Vect as Vect

data AnimationLayer = AnimationLayer {
  layer :: Layer.Layer,
  counter :: Int,
  tileSize :: Vect.V2 Int,
  scaler :: Int,
  config :: Config
}

instance ComponentUnit AnimationLayer where
  update = execStateT $ do
    modify $ \c -> c { counter = (counter c + 1) }
    modify $ \c -> c { counter = if counter c >= (division (config c) ^._x * division (config c) ^. _y) * scaler c then 0 else counter c }

  figures comp = do
    let iv = V2 ((counter comp `div` scaler comp) `mod` division (config comp) ^. _x) ((counter comp `div` scaler comp) `div` division (config comp) ^. _x)
    return [
      clip (SDL.Rectangle (SDL.P (tileSize comp * iv)) (tileSize comp)) $ Layer.layer $ layer comp
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
  let size = getFigureSize (Layer.layer layer)

  return $ AnimationLayer
    { layer    = layer
    , counter  = 0
    , tileSize = div <$> size <*> division conf
    , scaler   = 25
    , config   = conf
    }