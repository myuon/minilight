{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
module MiniLight.Light (
  HasLightEnv (..),
  LightT (..),
  LightEnv (..),
  MiniLight,
  liftMiniLight,
  envLightT,
  mapLightT,

  HasLoopEnv (..),
  LoopEnv (..),

  FontDescriptor(..),
  FontStyle(..),
  loadFontCache,
  loadFont,
  withFont,

  -- * Re-exports
  MonadIO(..),
) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class
import qualified Control.Monad.Caster as Caster
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Graphics.Text.TrueType
import MiniLight.Event
import qualified SDL
import qualified SDL.Font

instance Hashable FontDescriptor where
  hashWithSalt n fd = let style = _descriptorStyle fd in hashWithSalt n (_descriptorFamilyName fd, _fontStyleBold style, _fontStyleItalic style)


type FontMap = HM.HashMap FontDescriptor FilePath

-- | The environment for LightT monad.
data LightEnv = LightEnv
  { renderer :: Maybe SDL.Renderer  -- ^ Renderer for SDL2
  , fontCache :: FontMap  -- ^ System font information
  , logger :: Caster.LogQueue  -- ^ Logger connected stdout
  }

makeClassy_ ''LightEnv

data LoopEnv = LoopEnv
  { keyStates :: HM.HashMap SDL.Scancode Int  -- ^ Current state of keys, represents how many frames the key down has been down
  , events :: MVar [Event]  -- ^ Event queue
  , signalQueue :: IORef [Event]  -- ^ Signals emitted from components are stored in the queue and poll in the next frame.
  }

makeClassy_ ''LoopEnv


newtype LightT env m a = LightT { runLightT' :: ReaderT env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadMask, MonadCatch)

instance Monad m => MonadReader env (LightT env m) where
  ask = LightT ask
  local f = LightT . local f . runLightT'

instance (Monad m, HasLightEnv env) => Caster.MonadLogger (LightT env m) where
  getLogger = view _logger

type MiniLight = LightT LightEnv IO

liftMiniLight :: (HasLightEnv env, MonadIO m) => MiniLight a -> LightT env m a
liftMiniLight m = do
  env <- view lightEnv

  LightT $ ReaderT $ \_ -> liftIO $ runReaderT (runLightT' m) env
{-# INLINE liftMiniLight #-}

envLightT :: (env' -> env) -> LightT env m a -> LightT env' m a
envLightT f m = LightT $ ReaderT $ runReaderT (runLightT' m) . f
{-# INLINE envLightT #-}

mapLightT :: (m a -> n a) -> LightT env m a -> LightT env n a
mapLightT f m = LightT $ ReaderT $ f . runReaderT (runLightT' m)
{-# INLINE mapLightT #-}

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
  fc <- view _fontCache
  let path = fc HM.! fd
  SDL.Font.load path size

withFont
  :: (HasLightEnv env, MonadIO m, MonadMask m)
  => FontDescriptor
  -> Int
  -> (SDL.Font.Font -> LightT env m a)
  -> LightT env m a
withFont fd n = bracket (loadFont fd n) SDL.Font.free
