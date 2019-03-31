{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
module MiniLight.Light (
  HasLightEnv (..),
  LightT (..),
  LightEnv (..),
  MiniLight,
  liftMiniLight,
  transEnvLightT
) where

import Control.Monad.Catch
import Control.Monad.Reader
import Lens.Micro
import Lens.Micro.Mtl
import qualified SDL

class HasLightEnv env where
  rendererL :: Lens' env SDL.Renderer

newtype LightT env m a = LightT { runLightT' :: ReaderT env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadMask, MonadCatch)

instance Monad m => MonadReader env (LightT env m) where
  ask = LightT ask
  local f = LightT . local f . runLightT'

data LightEnv = LightEnv
  { renderer :: SDL.Renderer
  }

instance HasLightEnv LightEnv where
  rendererL = lens renderer (\env r -> env { renderer = r })

type MiniLight = LightT LightEnv IO

liftMiniLight :: (HasLightEnv env, MonadIO m) => MiniLight a -> LightT env m a
liftMiniLight m = do
  renderer <- view rendererL
  LightT $ ReaderT $ \_ ->
    liftIO $ runReaderT (runLightT' m) (LightEnv {renderer = renderer})

transEnvLightT :: (env' -> env) -> LightT env m a -> LightT env' m a
transEnvLightT f m = LightT $ ReaderT $ runReaderT (runLightT' m) . f


