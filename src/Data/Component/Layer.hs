module Data.Component.Layer where

import Control.Monad
import Data.Aeson
import Lens.Micro
import Lens.Micro.Mtl
import Linear
import MiniLight
import qualified SDL
import qualified SDL.Vect as Vect
import qualified Data.Component.Basic as Basic

data Layer = Layer {
  layer :: Figure,
  config :: Config
}

instance ComponentUnit Layer where
  figures comp = return $ Basic.wrapFigures (basic $ config comp) [layer comp]

data Config = Config {
  basic :: Basic.Config,
  image :: FilePath
}

instance FromJSON Config where
  parseJSON = Basic.wrapConfig (\b l -> return $ Config b l) $ \v ->
    v .: "image"

new :: Config -> MiniLight Layer
new conf = do
  pic <- picture (image conf)

  return $ Layer
    { layer  = pic
    , config = conf { basic = (basic conf) { Basic.size = getFigureSize pic } }
    }

newNineTile :: Config -> MiniLight Layer
newNineTile conf = do
  pic <- picture $ image conf
  let siz     = fmap toEnum $ Basic.size $ basic conf
  let tex     = texture pic
  let texSize = fmap toEnum $ getFigureSize pic

  tinfo    <- SDL.queryTexture tex
  renderer <- view rendererL

  target   <- SDL.createTexture renderer
                                (SDL.texturePixelFormat tinfo)
                                SDL.TextureAccessTarget
                                siz
  SDL.rendererRenderTarget renderer SDL.$= Just target
  SDL.textureBlendMode target SDL.$= SDL.BlendAlphaBlend

  let tileSize = fmap (`div` 3) texSize

  forM_ [0 .. 2] $ \ix -> forM_ [0 .. 2] $ \iy -> do
    let targetSize = V2
          (if ix == 1 then siz ^. _x - 2 * tileSize ^. _x else tileSize ^. _x)
          (if iy == 1 then siz ^. _y - 2 * tileSize ^. _y else tileSize ^. _y)
    let targetLoc = V2
          ( if ix == 0
            then 0
            else if ix == 1 then tileSize ^. _x else siz ^. _x - tileSize ^. _x
          )
          ( if iy == 0
            then 0
            else if iy == 1 then tileSize ^. _y else siz ^. _y - tileSize ^. _y
          )

    SDL.copy
      renderer
      tex
      (Just $ SDL.Rectangle (SDL.P (tileSize * Vect.V2 ix iy)) tileSize)
      (Just $ SDL.Rectangle (SDL.P targetLoc) targetSize)

  SDL.rendererRenderTarget renderer SDL.$= Nothing

  tex <- fromTexture target
  return $ Layer {layer = tex, config = conf}

