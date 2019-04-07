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
  engine :: CME.MessageEngine,
  layer :: CLayer.Layer,
  cursor :: CAnim.AnimationLayer,
  config :: Config
}

engineL :: Lens' MessageLayer CME.MessageEngine
engineL = lens engine (\s a -> s { engine = a })

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

  figures comp = fmap (map (translate $ position $ config comp)) $ do
    baseLayer <- figures $ layer comp
    cursorLayer <- figures $ cursor comp
    cursorLayerSize <- fmap (^. sizeL) $ getComponentSize $ cursor comp
    textLayer <- figures $ engine comp

    let windowSize = CLayer.size $ layerConf $ config comp

    return
      $ baseLayer
      ++ map (translate (Vect.V2 20 10)) textLayer
      ++ map (translate (Vect.V2 ((windowSize ^. _x - cursorLayerSize ^. _x) `div` 2) (windowSize ^. _y - cursorLayerSize ^. _y))) cursorLayer

data Config = Config {
  position :: Vect.V2 Int,
  messageEngineConf :: CME.Config,
  layerConf :: CLayer.Config,
  nextConf :: CAnim.Config
}

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    position <- withObject "position" (\v -> Vect.V2 <$> v .: "x" <*> v .: "y") =<< v .: "position"

    layerConf <- parseJSON =<< v .: "window"
    nextConf <- parseJSON =<< v .: "next"
    messageEngineConf <- parseJSON =<< v .: "engine"

    return $ Config position messageEngineConf layerConf nextConf

new :: Config -> MiniLight MessageLayer
new conf = do
  engine <- CME.new (messageEngineConf conf)
  layer  <- CLayer.newNineTile (layerConf conf)
  cursor <- CAnim.new (nextConf conf)

  return $ MessageLayer
    { engine = engine
    , layer  = layer
    , cursor = cursor
    , config = conf
    }
