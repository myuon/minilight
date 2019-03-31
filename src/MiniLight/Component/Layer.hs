module MiniLight.Component.Layer where

import Control.Monad
import Foreign.C.Types (CInt)
import Lens.Micro
import Lens.Micro.Mtl
import Linear
import MiniLight.Component.Types
import MiniLight.Light
import MiniLight.Figure
import qualified SDL
import qualified SDL.Vect as Vect

data Layer = Layer {
  layer :: Figure MiniLight
}

instance ComponentUnit Layer where
  draw comp = liftMiniLight $ render $ layer comp

new :: FilePath -> MiniLight Layer
new path = do
  return $ Layer {layer = picture path}

newNineTile :: FilePath -> Vect.V2 CInt -> MiniLight Layer
newNineTile path size = do
  let pic = picture path
  tex      <- getTexture pic
  texSize  <- getTextureSize pic
  tinfo    <- SDL.queryTexture tex
  renderer <- view rendererL

  target   <- SDL.createTexture renderer
                                (SDL.texturePixelFormat tinfo)
                                SDL.TextureAccessTarget
                                size
  SDL.rendererRenderTarget renderer SDL.$= Just target
  SDL.textureBlendMode target SDL.$= SDL.BlendAlphaBlend

  let tileSize = fmap (`div` 3) texSize

  forM_ [0 .. 2] $ \ix -> forM_ [0 .. 2] $ \iy -> do
    let targetSize = V2
          (if ix == 1 then size ^. _x - 2 * tileSize ^. _x else tileSize ^. _x)
          (if iy == 1 then size ^. _y - 2 * tileSize ^. _y else tileSize ^. _y)
    let targetLoc = V2
          ( if ix == 0
            then 0
            else if ix == 1 then tileSize ^. _x else size ^. _x - tileSize ^. _x
          )
          ( if iy == 0
            then 0
            else if iy == 1 then tileSize ^. _y else size ^. _y - tileSize ^. _y
          )

    SDL.copy
      renderer
      tex
      (Just $ SDL.Rectangle (SDL.P (tileSize * Vect.V2 ix iy)) tileSize)
      (Just $ SDL.Rectangle (SDL.P targetLoc) targetSize)

  SDL.rendererRenderTarget renderer SDL.$= Nothing

  tex <- fromTexture target
  return $ Layer {layer = tex}

