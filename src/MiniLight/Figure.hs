{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module MiniLight.Figure where

import Control.Monad.Catch
import Control.Monad.IO.Class
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

sizeL :: Lens' (SDL.Rectangle a) (Vect.V2 a)
sizeL = lens (\(SDL.Rectangle _ size) -> size)
             (\(SDL.Rectangle center _) size' -> SDL.Rectangle center size')

newtype Figure = Figure { getFigure :: forall env m r. (MonadIO m, MonadMask m, HasLightEnv env) => Vect.V4 Word8 -> (SDL.Texture -> SDL.Rectangle CInt -> SDL.Rectangle CInt -> LightT env m r) -> LightT env m r }

getTexture
  :: (HasLightEnv env, MonadIO m, MonadMask m)
  => Figure
  -> LightT env m (SDL.Texture)
getTexture fig = getFigure fig 0 (\x _ _ -> return x)

getFigureSize
  :: (HasLightEnv env, MonadIO m, MonadMask m)
  => Figure
  -> LightT env m (Vect.V2 CInt)
getFigureSize fig =
  getFigure fig 0 (\_ (SDL.Rectangle _ size) _ -> return size)

getFigureArea
  :: (HasLightEnv env, MonadIO m, MonadMask m)
  => Figure
  -> LightT env m (SDL.Rectangle Int)
getFigureArea fig = fmap (fmap fromEnum) $ getFigure fig 0 (\_ r _ -> return r)

union :: SDL.Rectangle Int -> SDL.Rectangle Int -> SDL.Rectangle Int
union x@(SDL.Rectangle (SDL.P c1) s1) y@(SDL.Rectangle (SDL.P c2) s2)
  | c1 < c2 = SDL.Rectangle (SDL.P (fmap (2 *) c1 - s1 + fmap (2 *) c2 + s2))
                            (c2 - c1 + fmap (`div` 2) (s1 + s2))
  | otherwise = union y x

fromTexture :: MonadIO m => SDL.Texture -> m Figure
fromTexture tex = do
  tinfo <- SDL.queryTexture tex
  let size = Vect.V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo)
  return $ Figure $ \_ k -> do
    k tex (SDL.Rectangle 0 size) (SDL.Rectangle 0 size)

render :: (HasLightEnv env, MonadIO m, MonadMask m) => Figure -> LightT env m ()
render fig = do
  renderer <- view rendererL

  let color = Vect.V4 255 255 255 255
  texture <- getFigure fig color (\x _ _ -> return x)
  srcArea <- getFigure fig color (\_ y _ -> return y)
  tgtArea <- getFigure fig color (\_ _ y -> return y)

  SDL.copy renderer texture (Just srcArea) (Just tgtArea)

renders
  :: (HasLightEnv env, MonadIO m, MonadMask m) => [Figure] -> LightT env m ()
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

instance Rendering Figure where
  translate v (Figure fig) =
    let cv = fmap toEnum v in
    Figure $ \color k -> fig color (\tex srcArea tgtArea -> k tex srcArea (centerL +~ cv $ tgtArea))

  colorize color (Figure fig) = Figure $ \_ -> fig color

  -- srcArea and tgtArea should be the same size
  clip (SDL.Rectangle (SDL.P point') size') (Figure fig) = Figure $ \color k -> do
    tex <- fig color (\x _ _ -> return x)
    srcArea <- fig color (\_ y _ -> return y)
    tgtArea <- fig color (\_ _ y -> return y)

    let SDL.Rectangle (SDL.P point) _ = srcArea
    let newSrcArea = (SDL.Rectangle (SDL.P $ point + fmap toEnum point') (fmap toEnum size'))
    let SDL.Rectangle p _ = tgtArea
    let newTgtArea = (SDL.Rectangle p (fmap toEnum size'))
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


