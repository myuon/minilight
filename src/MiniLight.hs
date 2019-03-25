{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
module MiniLight where

import Capability.Reader
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader hiding (MonadReader)
import qualified Data.Text as T
import GHC.Generics
import Lens.Micro
import qualified SDL
import qualified SDL.Font

data LightEnv = LightEnv {
  renderer :: SDL.Renderer
}
  deriving Generic

newtype LightT (m :: * -> *) a = LightT { unwrapLightT :: LightEnv -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask) via (ReaderT LightEnv m)

deriving
  via Field "renderer" "env" (MonadReader (ReaderT LightEnv m))
  instance Monad m => HasReader "renderer" SDL.Renderer (LightT m)

type MiniLight = LightT IO

runLightT
  :: (MonadIO m, MonadMask m)
  => LightT m a
  -> m a
runLightT prog = withSDL $ withWindow $ \window -> do
  rend <- SDL.createRenderer window (-1) SDL.defaultRenderer
  unwrapLightT prog $ LightEnv {renderer = rend}

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

