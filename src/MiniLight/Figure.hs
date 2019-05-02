{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | This module provides many convenient operations for textures.
module MiniLight.Figure where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Word (Word8)
import Linear (_x, _y)
import MiniLight.Light
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Primitive as Gfx
import qualified SDL.Vect as Vect

-- | Lens for the center of a rectangle.
centerL :: Lens' (SDL.Rectangle a) (Vect.V2 a)
centerL = lens
  (\(SDL.Rectangle (SDL.P center) _) -> center)
  (\(SDL.Rectangle _ size) center' -> SDL.Rectangle (SDL.P center') size)

-- | Lens for the size of a rectangle.
sizeL :: Lens' (SDL.Rectangle a) (Vect.V2 a)
sizeL = lens (\(SDL.Rectangle _ size) -> size)
             (\(SDL.Rectangle center _) size' -> SDL.Rectangle center size')

-- | Figure type carries a texture, sizing information and rotation information.
data Figure = Figure {
  texture :: SDL.Texture,
  sourceArea :: SDL.Rectangle Int,
  targetArea :: SDL.Rectangle Int,
  rotation :: Double
}

getFigureSize :: Figure -> Vect.V2 Int
getFigureSize fig = (\(SDL.Rectangle _ size) -> size) $ targetArea fig
{-# INLINE getFigureSize #-}

freeFigure :: MonadIO m => Figure -> m ()
freeFigure = SDL.destroyTexture . texture
{-# INLINE freeFigure #-}

union :: SDL.Rectangle Int -> SDL.Rectangle Int -> SDL.Rectangle Int
union x@(SDL.Rectangle (SDL.P c1) s1) y@(SDL.Rectangle (SDL.P c2) s2)
  | c1 <= c2 = SDL.Rectangle (SDL.P (fmap (2 *) c1 - s1 + fmap (2 *) c2 + s2))
                             (c2 - c1 + fmap (`div` 2) (s1 + s2))
  | otherwise = union y x

-- | Render a figure.
render :: (HasLightEnv env, MonadIO m, MonadMask m) => Figure -> LightT env m ()
render fig = do
  renderer <- view _renderer

  SDL.copyEx renderer
             (texture fig)
             (Just (fmap toEnum $ sourceArea fig))
             (Just (fmap toEnum $ targetArea fig))
             (realToFrac (rotation fig) * 180 / pi)
             Nothing
             (Vect.V2 False False)
{-# INLINE render #-}

-- | Render figures.
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

-- | Rendering typeclass provides basic operations for figures.
class Rendering r m | r -> m where
  -- | Change the place to be rendered.
  translate :: Vect.V2 Int -> r -> r

  -- | Specify some area and clip the figure into the region.
  clip :: SDL.Rectangle Int -> r -> r

  -- | Rotate a figure.
  rotate :: Double -> r -> r

  -- | Create a text texture. __Be careful__: this is a slow operation, use cache as long as you can.
  text :: SDL.Font.Font -> Vect.V4 Word8 -> T.Text -> m r

  -- | Create a texture from a png file. __Be careful__: this is a slow operation, use cache as long as you can.
  picture :: FilePath -> m r

  -- | Create a texture from a raw SDL texture.
  fromTexture :: SDL.Texture -> m r

  -- | Create an outlined rectangle. __Be careful__: this is a slow operation, use cache as long as you can.
  rectangleOutline
    :: Vect.V4 Word8  -- ^ Stroke color
    -> Vect.V2 Int  -- ^ Size
    -> m r

  -- | Create a filled texture. __Be careful__: this is a slow operation, use cache as long as you can.
  rectangleFilled
    :: Vect.V4 Word8  -- ^ Filling color
    -> Vect.V2 Int  -- ^ Size
    -> m r

  -- | Create an outlined triangle. __Be careful__: this is a slow operation, use cache as long as you can.
  triangleOutline
    :: Vect.V4 Word8  -- ^ Stroke color
    -> Vect.V2 Int  -- ^ Size
    -> m r

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

  rotate ang fig = fig { rotation = ang }
  {-# INLINE rotate #-}

  text font color txt = do
    renderer <- view _renderer

    withBlendedText font txt color $ \surf -> do
      texture <- SDL.createTextureFromSurface renderer surf
      tinfo <- SDL.queryTexture texture
      let rect = fmap fromEnum $ SDL.Rectangle (SDL.P 0) (Vect.V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo))

      return $ Figure texture rect rect 0
  {-# INLINE text #-}

  picture filepath = do
    renderer <- view _renderer

    texture <- SDL.Image.loadTexture renderer filepath
    tinfo <- SDL.queryTexture texture
    let rect = fmap fromEnum $ SDL.Rectangle (SDL.P 0) (Vect.V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo))

    return $ Figure texture rect rect 0
  {-# INLINE picture #-}

  fromTexture tex = do
    tinfo <- SDL.queryTexture tex
    let size = fmap fromEnum $ Vect.V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo)

    return $ Figure tex (SDL.Rectangle 0 size) (SDL.Rectangle 0 size) 0
  {-# INLINE fromTexture #-}

  rectangleOutline color size = do
    rend <- view _renderer
    tex <- SDL.createTexture rend SDL.RGBA8888 SDL.TextureAccessTarget (fmap toEnum size)
    SDL.textureBlendMode tex SDL.$= SDL.BlendAlphaBlend

    bracket (SDL.get (SDL.rendererRenderTarget rend)) (\target -> SDL.rendererRenderTarget rend SDL.$= target) $ \_ -> do
      SDL.rendererRenderTarget rend SDL.$= Just tex
      SDL.rendererDrawColor rend SDL.$= color
      SDL.drawRect rend (Just $ SDL.Rectangle 0 (fmap toEnum size))

    return $ Figure tex (SDL.Rectangle 0 size) (SDL.Rectangle 0 size) 0
  {-# INLINE rectangleOutline #-}

  rectangleFilled color size = do
    rend <- view _renderer
    tex <- SDL.createTexture rend SDL.RGBA8888 SDL.TextureAccessTarget (fmap toEnum size)
    SDL.textureBlendMode tex SDL.$= SDL.BlendAlphaBlend

    bracket (SDL.get (SDL.rendererRenderTarget rend)) (\target -> SDL.rendererRenderTarget rend SDL.$= target) $ \_ -> do
      SDL.rendererRenderTarget rend SDL.$= Just tex
      SDL.rendererDrawColor rend SDL.$= color
      SDL.fillRect rend (Just $ SDL.Rectangle 0 (fmap toEnum size))

    return $ Figure tex (SDL.Rectangle 0 size) (SDL.Rectangle 0 size) 0
  {-# INLINE rectangleFilled #-}

  triangleOutline color size = do
    rend <- view _renderer
    tex <- SDL.createTexture rend SDL.RGBA8888 SDL.TextureAccessTarget (fmap toEnum size)
    SDL.textureBlendMode tex SDL.$= SDL.BlendAlphaBlend

    bracket (SDL.get (SDL.rendererRenderTarget rend)) (\target -> SDL.rendererRenderTarget rend SDL.$= target) $ \_ -> do
      SDL.rendererRenderTarget rend SDL.$= Just tex
      SDL.rendererDrawColor rend SDL.$= color

      let size' = fmap toEnum size
      Gfx.smoothTriangle
        rend
        (Vect.V2 (size' ^. _x `div` 2) 0)
        (Vect.V2 (size' ^. _x - 1) (size' ^. _y - 1))
        (Vect.V2 0 (size' ^. _y - 1))
        color

    return $ Figure tex (SDL.Rectangle 0 size) (SDL.Rectangle 0 size) 0
  {-# INLINE triangleOutline #-}
