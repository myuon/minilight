module MiniLight.Component.MessageLayer where

import Control.Monad.State
import Data.Aeson
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Mtl
import Linear
import MiniLight.Component.Types
import qualified MiniLight.Component.Layer as CLayer
import qualified MiniLight.Component.AnimationLayer as CAnim
import MiniLight.Figure
import MiniLight.Light
import qualified SDL
import qualified SDL.Font
import qualified SDL.Vect as Vect

data MessageEngine = MessageEngine {
  font :: SDL.Font.Font,
  counter :: Int,
  message :: T.Text,
  rendered :: Int,
  textTexture :: Figure,
  layer :: CLayer.Layer,
  cursor :: CAnim.AnimationLayer,
  finished :: Bool,
  config :: Config
}

cursorL :: Lens' MessageEngine CAnim.AnimationLayer
cursorL = lens cursor (\s a -> s { cursor = a })

instance ComponentUnit MessageEngine where
  update = execStateT $ do
    comp <- get

    unless (finished comp) $ do
      when (counter comp `mod` 10 == 0) $ do
        id %= (\c -> c { rendered = rendered c + 1 })

        comp <- get
        when (rendered comp == T.length (message comp)) $ do
          id %= (\c -> c { finished = True })

      id %= (\c -> c { counter = counter c + 1 })

      zoom cursorL $ do
        c <- use id
        id <~ lift (update c)

  figures comp = fmap (map (translate $ position $ config comp)) $ do
    baseLayer <- figures $ layer comp
    cursorLayer <- figures $ cursor comp
    cursorLayerSize <- fmap (^. sizeL) $ getComponentSize $ cursor comp
    (w, h) <- SDL.Font.size (font comp) (T.take (rendered comp) $ message comp)
    let windowSize = size $ config comp

    return
      $ baseLayer
      ++ [ translate (Vect.V2 20 10) $ colorize (Vect.V4 255 255 255 255) $ clip (SDL.Rectangle 0 (Vect.V2 w h)) $ textTexture comp ]
      ++ map (translate (Vect.V2 ((windowSize ^. _x - cursorLayerSize ^. _x) `div` 2) (windowSize ^. _y - cursorLayerSize ^. _y))) cursorLayer

data Config = Config {
  size :: Vect.V2 Int,
  position :: Vect.V2 Int,
  layerImage :: FilePath,
  waitingImage :: FilePath,
  waitingImageDivision :: Vect.V2 Int,
  messages :: T.Text,
  static :: Bool
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

    messages <- v .: "messages"

    static <- v .:? "static" .!= False

    return $ Config size position layerImage waitingImage waitingImageDivision messages static

new :: SDL.Font.Font -> Config -> MiniLight MessageEngine
new font conf = do
  layer  <- CLayer.newNineTile (layerImage conf) (fmap toEnum $ size conf)
  cursor <- CAnim.new (waitingImage conf) (waitingImageDivision conf)

  return $ MessageEngine
    { font        = font
    , counter     = 0
    , message     = messages conf
    , rendered    = if static conf then T.length (messages conf) else 0
    , textTexture = text font $ messages conf
    , layer       = layer
    , cursor      = cursor
    , finished    = static conf
    , config      = conf
    }


