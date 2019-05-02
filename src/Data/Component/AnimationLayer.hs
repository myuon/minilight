module Data.Component.AnimationLayer where

import Control.Lens
import Control.Monad.State
import Data.Aeson
import Linear
import MiniLight
import qualified Data.Component.Layer as Layer
import qualified SDL
import qualified SDL.Vect as Vect

data AnimationLayer = AnimationLayer {
  layer :: Layer.Layer,
  counter :: Int,
  tileSize :: Vect.V2 Int,
  config :: Config
}

instance ComponentUnit AnimationLayer where
  update = execStateT $ do
    modify $ \c -> c { counter = (counter c + 1) }
    modify $ \c -> c { counter = if counter c >= (division (config c) ^._x * division (config c) ^. _y) * interval (config c) then 0 else counter c }

  figures comp = do
    let iv = V2 ((counter comp `div` interval (config comp)) `mod` division (config comp) ^. _x) ((counter comp `div` interval (config comp)) `div` division (config comp) ^. _x)
    return [
      clip (SDL.Rectangle (SDL.P (tileSize comp * iv)) (tileSize comp)) $ Layer.layer $ layer comp
      ]

data Config = Config {
  layerConf :: Layer.Config,
  division :: Vect.V2 Int,
  interval :: Int
}

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    conf <- parseJSON (Object v)
    division <- (\v -> Vect.V2 <$> v .: "x" <*> v .: "y") =<< v .: "division"
    interval <- v .:? "interval" .!= 30

    return $ Config conf division interval

new :: Config -> MiniLight AnimationLayer
new conf = do
  layer <- Layer.new (layerConf conf)
  let size = getFigureSize (Layer.layer layer)

  return $ AnimationLayer
    { layer    = layer
    , counter  = 0
    , tileSize = div <$> size <*> division conf
    , config   = conf
    }
