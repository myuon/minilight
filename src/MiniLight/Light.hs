{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
module MiniLight.Light (
  HasLightEnv (..),
  LightT (..),
  LightEnv (..),
  MiniLight,
  liftMiniLight,
  transEnvLightT,

  FontDescriptor(..),
  loadFontCache,
  loadFont,
  withFont
) where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Graphics.Text.TrueType
import Lens.Micro
import Lens.Micro.Mtl
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

transEnvLightT :: (env' -> env) -> LightT env m a -> LightT env' m a
transEnvLightT f m = LightT $ ReaderT $ runReaderT (runLightT' m) . f

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
