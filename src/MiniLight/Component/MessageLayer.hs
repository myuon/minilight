module MiniLight.Component.MessageLayer where

import Control.Monad.State
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Mtl
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
  textTexture :: Figure MiniLight,
  layer :: CLayer.Layer,
  cursor :: CAnim.AnimationLayer,
  config :: Config
}

cursorL :: Lens' MessageEngine CAnim.AnimationLayer
cursorL = lens cursor (\s a -> s { cursor = a })

instance ComponentUnit MessageEngine where
  update = execStateT $ do
    comp <- get

    when (counter comp `mod` 10 == 0) $ do
      id %= (\c -> c { rendered = rendered c + 1 })

    id %= (\c -> c { counter = counter c + 1 })

    zoom cursorL $ do
      c <- use id
      id <~ lift (update c)

  draw comp = do
    draw $ layer comp

    (w, h) <- SDL.Font.size (font comp) (T.take (rendered comp) $ message comp)
    liftMiniLight
      $ renders [colorize (Vect.V4 0 0 0 255) $ clip (SDL.Rectangle 0 (Vect.V2 w h)) $ textTexture comp]

    draw $ cursor comp

data Config = Config {
  size :: Vect.V2 Int,
  layerImage :: FilePath,
  waitingImage :: FilePath,
  waitingImageDivision :: Vect.V2 Int
}

new :: SDL.Font.Font -> T.Text -> Config -> MiniLight MessageEngine
new font message conf = do
  layer  <- CLayer.newNineTile (layerImage conf) (fmap toEnum $ size conf)
  cursor <- CAnim.new (waitingImage conf) (waitingImageDivision conf)

  return $ MessageEngine
    { font        = font
    , counter     = 0
    , message     = message
    , rendered    = 0
    , textTexture = text font message
    , layer       = layer
    , cursor      = cursor
    , config      = conf
    }


