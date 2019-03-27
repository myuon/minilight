module MiniLight (
  module MiniLight.Figure,
  module MiniLight.Light,

  MiniLight,
  runLightT,

  withSDL,
  withWindow,
  withFont,
  withBlendedText
) where

import Control.Monad.Reader
import Control.Monad.Catch
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Mtl
import MiniLight.Light
import MiniLight.Figure
import qualified SDL
import qualified SDL.Font
import qualified SDL.Vect as Vect

data LightEnv = LightEnv {
  renderer :: SDL.Renderer
}

instance HasLightEnv LightEnv where
  rendererL = lens renderer (\env r -> env { renderer = r })

type MiniLight = LightT LightEnv IO

runLightT
  :: (HasLightEnv env, MonadIO m, MonadMask m)
  => (LightEnv -> env)
  -> LightT env m a
  -> m a
runLightT init prog = withSDL $ withWindow $ \window -> do
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  runReaderT (runLightT' prog) $ init $ LightEnv {renderer = renderer}

--

withSDL :: (MonadIO m, MonadMask m) => m a -> m a
withSDL =
  bracket (SDL.initializeAll >> SDL.Font.initialize)
          (\_ -> SDL.Font.quit >> SDL.quit)
    . const

withWindow :: (MonadIO m, MonadMask m) => (SDL.Window -> m a) -> m a
withWindow =
  bracket (SDL.createWindow "window" SDL.defaultWindow) SDL.destroyWindow

withFont
  :: (MonadIO m, MonadMask m) => FilePath -> (SDL.Font.Font -> m a) -> m a
withFont path = bracket (SDL.Font.load path 22) SDL.Font.free

withBlendedText
  :: (MonadIO m, MonadMask m)
  => SDL.Font.Font
  -> T.Text
  -> SDL.Font.Color
  -> (SDL.Surface -> m a)
  -> m a
withBlendedText font text color =
  bracket (SDL.Font.blended font color text) SDL.freeSurface

instance Rendering (Figure MiniLight) where
  translate v fig =
    let cv = fmap toEnum v in
    Figure $ \color k -> getFigure fig color (\tex area -> k tex (centerL +~ cv $ area))

  colorize color fig = Figure $ \_ -> getFigure fig color

  text font txt = Figure $ \color k -> do
    renderer <- view rendererL

    withBlendedText font txt color $ \surf -> do
      texture <- SDL.createTextureFromSurface renderer surf
      tinfo <- SDL.queryTexture texture
      k texture (SDL.Rectangle (SDL.P 0) (Vect.V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo)))


