module MiniLight.Layers where

import Control.Monad
import Control.Monad.IO.Class
import Foreign.C.Types (CInt)
import Lens.Micro
import Lens.Micro.Mtl
import Linear
import MiniLight.Light
import MiniLight.Figure
import qualified SDL
import qualified SDL.Vect as Vect

newWindowLayer
  :: (HasLightEnv env, MonadIO m, Rendering (Figure (LightT env m)))
  => FilePath
  -> Vect.V2 CInt
  -> LightT env m SDL.Texture
newWindowLayer path size = do
  texture  <- createFigureTexture $ picture path
  tinfo    <- SDL.queryTexture texture
  renderer <- view rendererL

  target   <- SDL.createTexture renderer
                                (SDL.texturePixelFormat tinfo)
                                SDL.TextureAccessTarget
                                size
  SDL.rendererRenderTarget renderer SDL.$= Just target
  SDL.textureBlendMode target SDL.$= SDL.BlendAlphaBlend

  let tileSize =
        V2 (SDL.textureWidth tinfo `div` 3) (SDL.textureHeight tinfo `div` 3)

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
      texture
      (Just $ SDL.Rectangle (SDL.P (tileSize * Vect.V2 ix iy)) tileSize)
      (Just $ SDL.Rectangle (SDL.P targetLoc) targetSize)

  SDL.rendererRenderTarget renderer SDL.$= Nothing
  SDL.destroyTexture texture

  return target

