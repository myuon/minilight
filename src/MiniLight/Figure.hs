{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module MiniLight.Figure where

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
import qualified SDL.Vect as Vect

centerL :: Lens' (SDL.Rectangle a) (Vect.V2 a)
centerL = lens
  (\(SDL.Rectangle (SDL.P center) _) -> center)
  (\(SDL.Rectangle _ size) center' -> SDL.Rectangle (SDL.P center') size)

newtype Figure m = Figure { getFigure :: forall r. Vect.V4 Word8 -> (SDL.Texture -> SDL.Rectangle CInt -> m r) -> m r }

render
  :: (HasLightEnv env, MonadIO m) => Figure (LightT env m) -> LightT env m ()
render fig = do
  renderer <- view rendererL

  let color = Vect.V4 255 255 255 255
  texture <- getFigure fig color (\x _ -> return x)
  area    <- getFigure fig color (\_ y -> return y)

  SDL.copy renderer texture Nothing $ Just $ area
  SDL.destroyTexture texture

renders
  :: (HasLightEnv env, MonadIO m) => [Figure (LightT env m)] -> LightT env m ()
renders = mapM_ render

class Rendering r where
  translate :: Vect.V2 Int -> r -> r
  colorize :: Vect.V4 Word8 -> r -> r

  text :: SDL.Font.Font -> T.Text -> r
