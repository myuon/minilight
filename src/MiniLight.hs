{-# LANGUAGE DerivingVia #-}
module MiniLight where

import Control.Monad.Reader
import Control.Monad.Catch
import qualified Data.Text as T
import Lens.Micro
import qualified SDL
import qualified SDL.Font

data LightEnv = LightEnv {
  renderer :: SDL.Renderer
}

class HasLightEnv env where
  rendererL :: Lens' env SDL.Renderer

instance HasLightEnv LightEnv where
  rendererL = lens renderer (\env r -> env { renderer = r })

newtype LightT env m a = LightT { runLightT' :: ReaderT env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow) via (ReaderT env m)

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



