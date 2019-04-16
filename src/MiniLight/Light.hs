{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
module MiniLight.Light (
  HasLightEnv (..),
  LightT (..),
  LightEnv (..),
  MiniLight,
  liftMiniLight,
  envLightT,
  mapLightT,

  HasLoopEnv (..),
  emit,

  FontDescriptor(..),
  FontStyle(..),
  loadFontCache,
  loadFont,
  withFont,

  -- * Re-exports
  MonadIO(..),
) where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Text as T
import Graphics.Text.TrueType
import Lens.Micro
import Lens.Micro.Mtl
import MiniLight.Event
import qualified SDL
import qualified SDL.Font

type FontMap = HM.HashMap FontDescriptor FilePath

instance Hashable FontDescriptor where
  hashWithSalt n fd = let style = _descriptorStyle fd in hashWithSalt n (_descriptorFamilyName fd, _fontStyleBold style, _fontStyleItalic style)

class HasLightEnv env where
  rendererL :: Lens' env SDL.Renderer
  fontCacheL :: Lens' env FontMap

newtype LightT env m a = LightT { runLightT' :: ReaderT env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadMask, MonadCatch)

instance Monad m => MonadReader env (LightT env m) where
  ask = LightT ask
  local f = LightT . local f . runLightT'

data LightEnv = LightEnv
  { renderer :: SDL.Renderer
  , fontCache :: FontMap
  }

instance HasLightEnv LightEnv where
  rendererL = lens renderer (\env r -> env { renderer = r })
  fontCacheL = lens fontCache (\env r -> env { fontCache = r })

type MiniLight = LightT LightEnv IO

liftMiniLight :: (HasLightEnv env, MonadIO m) => MiniLight a -> LightT env m a
liftMiniLight m = do
  renderer  <- view rendererL
  fontCache <- view fontCacheL

  LightT $ ReaderT $ \_ -> liftIO $ runReaderT
    (runLightT' m)
    (LightEnv {renderer = renderer, fontCache = fontCache})
{-# INLINE liftMiniLight #-}

envLightT :: (env' -> env) -> LightT env m a -> LightT env' m a
envLightT f m = LightT $ ReaderT $ runReaderT (runLightT' m) . f
{-# INLINE envLightT #-}

mapLightT :: (m a -> n a) -> LightT env m a -> LightT env n a
mapLightT f m = LightT $ ReaderT $ f . runReaderT (runLightT' m)
{-# INLINE mapLightT #-}

class HasLoopEnv env where
  -- | Contains the number of frames that a specific keys are continuously pressing.
  keyStatesL :: Lens' env (HM.HashMap SDL.Scancode Int)

  -- | Occurred events since the last frame.
  eventsL :: Lens' env (IORef [Event])

  -- | A queue storing the events occurred in this frame.
  signalQueueL :: Lens' env (IORef [Event])

-- | Emit a signal, which will be catched at the next frame.
emit
  :: (HasLoopEnv env, MonadIO m, EventType et)
  => T.Text
  -> et
  -> LightT env m ()
emit name t = do
  ref <- view signalQueueL
  liftIO $ modifyIORef' ref $ (signal name t :)

loadFontCache :: MonadIO m => m FontMap
loadFontCache = do
  fc <- liftIO buildCache
  return $ foldl
    ( \fm fd -> HM.insert
      fd
      (maybe (error $ "Font not found: " ++ show fd) id (findFontInCache fc fd))
      fm
    )
    HM.empty
    (enumerateFonts fc)

loadFont
  :: (HasLightEnv env, MonadIO m)
  => FontDescriptor
  -> Int
  -> LightT env m SDL.Font.Font
loadFont fd size = do
  fc <- view fontCacheL
  let path = fc HM.! fd
  SDL.Font.load path size

withFont
  :: (HasLightEnv env, MonadIO m, MonadMask m)
  => FontDescriptor
  -> Int
  -> (SDL.Font.Font -> LightT env m a)
  -> LightT env m a
withFont fd n = bracket (loadFont fd n) SDL.Font.free
