module MiniLight.Component.MessageEngine where

import Control.Monad.State
import qualified Data.Text as T
import Lens.Micro.Mtl
import MiniLight.Component.Types
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
  textTexture :: Figure MiniLight
}

instance ComponentUnit MessageEngine where
  update = execStateT $ do
    comp <- get

    when (counter comp `mod` 10 == 0) $ do
      id %= (\c -> c { rendered = rendered c + 1 })

    id %= (\c -> c { counter = counter c + 1 })

  draw comp = do
    (w, h) <- SDL.Font.size (font comp) (T.take (rendered comp) $ message comp)
    liftMiniLight
      $ renders [colorize (Vect.V4 0 0 0 255) $ clip (SDL.Rectangle 0 (Vect.V2 w h)) $ textTexture comp]

