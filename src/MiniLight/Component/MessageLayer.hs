module MiniLight.Component.MessageLayer where

import Control.Monad.State
import Data.Aeson
import Lens.Micro
import Lens.Micro.Mtl
import Linear
import MiniLight.Component.Types
import qualified MiniLight.Component.Layer as CLayer
import qualified MiniLight.Component.AnimationLayer as CAnim
import qualified MiniLight.Component.MessageEngine as CME
import MiniLight.Figure
import MiniLight.Light
import qualified SDL.Vect as Vect

data MessageLayer = MessageLayer {
  messageEngine :: CME.MessageEngine,
  layer :: CLayer.Layer,
  cursor :: CAnim.AnimationLayer,
  config :: Config
}

engineL :: Lens' MessageLayer CME.MessageEngine
engineL = lens messageEngine (\s a -> s { messageEngine = a })

cursorL :: Lens' MessageLayer CAnim.AnimationLayer
cursorL = lens cursor (\s a -> s { cursor = a })

instance ComponentUnit MessageLayer where
  update = execStateT $ do
    zoom engineL $ do
      c <- use id
      id <~ lift (update c)

    zoom cursorL $ do
      c <- use id
      id <~ lift (update c)

  figures comp = do
    baseLayer <- figures $ layer comp
    cursorLayer <- figures $ cursor comp
    cursorLayerSize <- fmap (^. sizeL) $ getComponentSize $ cursor comp
    textLayer <- figures $ messageEngine comp

    let windowSize = CLayer.size $ window $ config comp

    return
      $ map (translate (CLayer.position (window (config comp)))) $ baseLayer
      ++ map (translate (Vect.V2 20 10)) textLayer
      ++ map (translate (Vect.V2 ((windowSize ^. _x - cursorLayerSize ^. _x) `div` 2) (windowSize ^. _y - cursorLayerSize ^. _y))) cursorLayer

data Config = Config {
  engine :: CME.Config,
  window :: CLayer.Config,
  next :: CAnim.Config
}

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    layerConf <- parseJSON =<< v .: "window"
    nextConf <- parseJSON =<< v .: "next"
    messageEngineConf <- parseJSON =<< v .: "engine"

    return $ Config messageEngineConf layerConf nextConf

new :: Config -> MiniLight MessageLayer
new conf = do
  engine <- CME.new (engine conf)
  layer  <- CLayer.newNineTile (window conf)
  cursor <- CAnim.new (next conf)

  return $ MessageLayer
    { messageEngine = engine
    , layer         = layer
    , cursor        = cursor
    , config        = conf
    }
