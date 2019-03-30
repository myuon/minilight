module MiniLight (
  module MiniLight.Figure,
  module MiniLight.Light,
  module MiniLight.Layers,

  LightEnv,
  MiniLight,
  runLightT,
  liftMiniLight,
  transEnvLightT,

  withSDL,
  withWindow,
  withFont,
  withBlendedText
) where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.IO.Unlift
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Mtl
import MiniLight.Light
import MiniLight.Figure
import MiniLight.Layers
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Vect as Vect

data LightEnv = LightEnv
  { renderer :: SDL.Renderer
  , resourceMap :: ResourceMap
  }

instance HasLightEnv LightEnv where
  rendererL = lens renderer (\env r -> env { renderer = r })
  resourceMapL = lens resourceMap (\env r -> env { resourceMap = r })

runLightT
  :: (HasLightEnv env, MonadIO m, MonadMask m, MonadUnliftIO m)
  => (LightEnv -> env)
  -> LightT env m a
  -> m a
runLightT init prog = withSDL $ withWindow $ \window -> do
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  withResourceMap $ \rmap -> runReaderT (runLightT' prog) $ init $ LightEnv
    { renderer    = renderer
    , resourceMap = rmap
    }

type MiniLight = LightT LightEnv IO

liftMiniLight :: (HasLightEnv env, MonadIO m) => MiniLight a -> LightT env m a
liftMiniLight m = do
  renderer    <- view rendererL
  resourceMap <- view resourceMapL
  LightT $ ReaderT $ \env -> liftIO $ runReaderT
    (runLightT' m)
    (LightEnv {renderer = renderer, resourceMap = resourceMap})

transEnvLightT :: (env' -> env) -> LightT env m a -> LightT env' m a
transEnvLightT f m = LightT $ ReaderT $ runReaderT (runLightT' m) . f

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

  picture filepath = Figure $ \_ k -> do
    renderer <- view rendererL

    texture <- SDL.Image.loadTexture renderer filepath
    tinfo <- SDL.queryTexture texture
    k texture (SDL.Rectangle (SDL.P 0) (Vect.V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo)))

  texture (Texture (tex, size)) = Figure $ \_ k -> do
    k tex (SDL.Rectangle (SDL.P 0) size)

