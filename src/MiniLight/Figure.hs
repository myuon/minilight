{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module MiniLight.Figure where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Reflection
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Lens.Micro
import Lens.Micro.Mtl
import MiniLight.Light
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Vect as Vect

centerL :: Lens' (SDL.Rectangle a) (Vect.V2 a)
centerL = lens
  (\(SDL.Rectangle (SDL.P center) _) -> center)
  (\(SDL.Rectangle _ size) center' -> SDL.Rectangle (SDL.P center') size)

newtype Figure m = Figure {
  getFigure :: forall r. Vect.V4 Word8 -> (SDL.Texture -> SDL.Rectangle CInt -> SDL.Rectangle CInt -> m r) -> m r
}

getTexture :: MonadIO m => Figure m -> m (SDL.Texture)
getTexture fig = getFigure fig 0 (\x _ _ -> return x)

getTextureSize :: MonadIO m => Figure m -> m (Vect.V2 CInt)
getTextureSize fig =
  getFigure fig 0 (\_ (SDL.Rectangle _ size) _ -> return size)

fromTexture :: MonadIO m => SDL.Texture -> m (Figure m)
fromTexture tex = do
  tinfo <- SDL.queryTexture tex
  let size = Vect.V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo)
  return $ Figure $ \color k -> do
    k tex (SDL.Rectangle 0 size) (SDL.Rectangle 0 size)

render
  :: (HasLightEnv env, MonadIO m) => Figure (LightT env m) -> LightT env m ()
render fig = do
  renderer <- view rendererL

  let color = Vect.V4 255 255 255 255
  texture <- getFigure fig color (\x _ _ -> return x)
  srcArea <- getFigure fig color (\_ y _ -> return y)
  tgtArea <- getFigure fig color (\_ _ y -> return y)

  SDL.copy renderer texture (Just srcArea) (Just tgtArea)

renders
  :: (HasLightEnv env, MonadIO m) => [Figure (LightT env m)] -> LightT env m ()
renders = mapM_ render

withBlendedText
  :: (MonadIO m, MonadMask m)
  => SDL.Font.Font
  -> T.Text
  -> SDL.Font.Color
  -> (SDL.Surface -> m a)
  -> m a
withBlendedText font text color =
  bracket (SDL.Font.blended font color text) SDL.freeSurface

class Rendering r where
  translate :: Vect.V2 Int -> r -> r
  colorize :: Vect.V4 Word8 -> r -> r
  clip :: SDL.Rectangle Int -> r -> r

  text :: SDL.Font.Font -> T.Text -> r
  picture :: FilePath -> r

instance Rendering (Figure MiniLight) where
  translate v fig =
    let cv = fmap toEnum v in
    Figure $ \color k -> getFigure fig color (\tex srcArea tgtArea -> k tex srcArea (centerL +~ cv $ tgtArea))

  colorize color fig = Figure $ \_ -> getFigure fig color

  -- srcArea and tgtArea should be the same size
  clip (SDL.Rectangle (SDL.P point') size') fig = Figure $ \color k -> do
    tex <- getFigure fig color (\x _ _ -> return x)
    srcArea <- getFigure fig color (\_ y _ -> return y)
    tgtArea <- getFigure fig color (\_ _ y -> return y)

    let SDL.Rectangle (SDL.P point) size = srcArea
    let newSrcArea = (SDL.Rectangle (SDL.P $ point + fmap toEnum point') (fmap toEnum size'))
    let SDL.Rectangle (SDL.P point) size = tgtArea
    let newTgtArea = (SDL.Rectangle (SDL.P $ point + fmap toEnum point') (fmap toEnum size'))
    k tex newSrcArea newTgtArea

  text font txt = Figure $ \color k -> do
    renderer <- view rendererL

    withBlendedText font txt color $ \surf -> do
      texture <- SDL.createTextureFromSurface renderer surf
      tinfo <- SDL.queryTexture texture
      let rect = SDL.Rectangle (SDL.P 0) (Vect.V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo))
      k texture rect rect

  picture filepath = Figure $ \_ k -> do
    renderer <- view rendererL

    texture <- SDL.Image.loadTexture renderer filepath
    tinfo <- SDL.queryTexture texture
    let rect = SDL.Rectangle (SDL.P 0) (Vect.V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo))
    k texture rect rect


