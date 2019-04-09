{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module MiniLight.Figure where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Word (Word8)
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

data Figure = Figure {
  texture :: SDL.Texture,
  sourceArea :: SDL.Rectangle Int,
  targetArea :: SDL.Rectangle Int
}

getFigureSize :: Figure -> Vect.V2 Int
getFigureSize fig = (\(SDL.Rectangle _ size) -> size) $ targetArea fig
{-# INLINE getFigureSize #-}

union :: SDL.Rectangle Int -> SDL.Rectangle Int -> SDL.Rectangle Int
union x@(SDL.Rectangle (SDL.P c1) s1) y@(SDL.Rectangle (SDL.P c2) s2)
  | c1 <= c2 = SDL.Rectangle (SDL.P (fmap (2 *) c1 - s1 + fmap (2 *) c2 + s2))
                             (c2 - c1 + fmap (`div` 2) (s1 + s2))
  | otherwise = union y x

render :: (HasLightEnv env, MonadIO m, MonadMask m) => Figure -> LightT env m ()
render fig = do
  renderer <- view rendererL

  SDL.copy renderer
           (texture fig)
           (Just (fmap toEnum $ sourceArea fig))
           (Just (fmap toEnum $ targetArea fig))
{-# INLINE render #-}

renders
  :: (HasLightEnv env, MonadIO m, MonadMask m) => [Figure] -> LightT env m ()
renders = mapM_ render
{-# INLINE renders #-}

withBlendedText
  :: (MonadIO m, MonadMask m)
  => SDL.Font.Font
  -> T.Text
  -> SDL.Font.Color
  -> (SDL.Surface -> m a)
  -> m a
withBlendedText font text color =
  bracket (SDL.Font.blended font color text) SDL.freeSurface
{-# INLINE withBlendedText #-}

class Rendering r m | r -> m where
  translate :: Vect.V2 Int -> r -> r
  clip :: SDL.Rectangle Int -> r -> r

  text :: SDL.Font.Font -> Vect.V4 Word8 -> T.Text -> m r
  picture :: FilePath -> m r
  fromTexture :: SDL.Texture -> m r
  rectangleOutline :: Vect.V4 Word8 -> Vect.V2 Int -> m r
  rectangleFilled :: Vect.V4 Word8 -> Vect.V2 Int -> m r

instance Rendering Figure MiniLight where
  translate v fig =
    let cv = fmap toEnum v in
    fig { targetArea = centerL +~ cv $ targetArea fig }
  {-# INLINE translate #-}

  -- srcArea and tgtArea should be the same size
  clip (SDL.Rectangle (SDL.P point') size') fig =
    let SDL.Rectangle (SDL.P point) _ = sourceArea fig;
        sourceArea' = (SDL.Rectangle (SDL.P $ point + fmap toEnum point') (fmap toEnum size'));
        SDL.Rectangle p _ = targetArea fig;
        targetArea' = (SDL.Rectangle p (fmap toEnum size'))
    in fig { sourceArea = sourceArea', targetArea = targetArea' }
  {-# INLINE clip #-}

  text font color txt = do
    renderer <- view rendererL

    withBlendedText font txt color $ \surf -> do
      texture <- SDL.createTextureFromSurface renderer surf
      tinfo <- SDL.queryTexture texture
      let rect = fmap fromEnum $ SDL.Rectangle (SDL.P 0) (Vect.V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo))

      return $ Figure texture rect rect
  {-# INLINE text #-}

  picture filepath = do
    renderer <- view rendererL

    texture <- SDL.Image.loadTexture renderer filepath
    tinfo <- SDL.queryTexture texture
    let rect = fmap fromEnum $ SDL.Rectangle (SDL.P 0) (Vect.V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo))

    return $ Figure texture rect rect
  {-# INLINE picture #-}

  fromTexture tex = do
    tinfo <- SDL.queryTexture tex
    let size = fmap fromEnum $ Vect.V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo)

    return $ Figure tex (SDL.Rectangle 0 size) (SDL.Rectangle 0 size)
  {-# INLINE fromTexture #-}

  rectangleOutline color size = do
    rend <- view rendererL
    tex <- SDL.createTexture rend SDL.RGBA8888 SDL.TextureAccessTarget (fmap toEnum size)
    SDL.textureBlendMode tex SDL.$= SDL.BlendAlphaBlend

    bracket (SDL.get (SDL.rendererRenderTarget rend)) (\target -> SDL.rendererRenderTarget rend SDL.$= target) $ \_ -> do
      SDL.rendererRenderTarget rend SDL.$= Just tex
      SDL.rendererDrawColor rend SDL.$= color
      SDL.drawRect rend (Just $ SDL.Rectangle 0 (fmap toEnum size))

    return $ Figure tex (SDL.Rectangle 0 size) (SDL.Rectangle 0 size)
  {-# INLINE rectangleOutline #-}

  rectangleFilled color size = do
    rend <- view rendererL
    tex <- SDL.createTexture rend SDL.RGBA8888 SDL.TextureAccessTarget (fmap toEnum size)
    SDL.textureBlendMode tex SDL.$= SDL.BlendAlphaBlend

    bracket (SDL.get (SDL.rendererRenderTarget rend)) (\target -> SDL.rendererRenderTarget rend SDL.$= target) $ \_ -> do
      SDL.rendererRenderTarget rend SDL.$= Just tex
      SDL.rendererDrawColor rend SDL.$= color
      SDL.fillRect rend (Just $ SDL.Rectangle 0 (fmap toEnum size))

    return $ Figure tex (SDL.Rectangle 0 size) (SDL.Rectangle 0 size)
  {-# INLINE rectangleFilled #-}
