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
import qualified SDL.Font
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

    let windowSize = size $ config comp

    return
      $ baseLayer
      ++ map (translate (Vect.V2 20 10)) textLayer
      ++ map (translate (Vect.V2 ((windowSize ^. _x - cursorLayerSize ^. _x) `div` 2) (windowSize ^. _y - cursorLayerSize ^. _y))) cursorLayer

data Config = Config {
  messageEngineConf :: CME.Config,
  size :: Vect.V2 Int,
  position :: Vect.V2 Int,
  layerImage :: FilePath,
  waitingImage :: FilePath,
  waitingImageDivision :: Vect.V2 Int
}

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    windowJSON <- v .: "window"
    (size, position, layerImage) <- flip (withObject "window") windowJSON $ \v -> do
      size <- withObject "size" (\v -> Vect.V2 <$> v .: "width" <*> v .: "height") =<< v .: "size"
      position <- withObject "position" (\v -> Vect.V2 <$> v .: "x" <*> v .: "y") =<< v .: "position"
      layerImage <- v .: "image"
      return (size, position, layerImage)

    nextJSON <- v .: "next"
    (waitingImage, waitingImageDivision) <- flip (withObject "image") nextJSON $ \v -> do
      waitingImage <- v .: "image"
      division <- (\v -> Vect.V2 <$> v .: "x" <*> v .: "y") =<< v .: "division"
      return (waitingImage, division)

    messageEngineConf <- v .: "engine"

    return $ Config messageEngineConf size position layerImage waitingImage waitingImageDivision

new :: SDL.Font.Font -> Config -> MiniLight MessageLayer
new font conf = do
  engine <- CME.new font (messageEngineConf conf)
  layer  <- CLayer.newNineTile (layerImage conf) (fmap toEnum $ size conf)
  cursor <- CAnim.new (waitingImage conf) (waitingImageDivision conf)

  return $ MessageLayer
    { engine = engine
    , layer  = layer
    , cursor = cursor
    , config = conf
    }


