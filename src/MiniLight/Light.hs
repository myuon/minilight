{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
module MiniLight.Light (
  ResourceMap,
  withResourceMap,

  HasLightEnv (..),
  LightT (..),
  LightEnv (..),
  MiniLight,
  liftMiniLight,
  transEnvLightT,

  ReleaseKey,
  allocate,
  register,
  release
) where

import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource (MonadResource(..), runResourceT, ReleaseKey, allocate, register, release)
import Control.Monad.Trans.Resource.Internal (ReleaseMap, ResourceT (..))
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map as M
import Lens.Micro
import Lens.Micro.Mtl
import qualified SDL

type ResourceMap = IORef ReleaseMap

class HasLightEnv env where
  rendererL :: Lens' env SDL.Renderer
  resourceMapL :: Lens' env ResourceMap

newtype LightT env m a = LightT { runLightT' :: ReaderT env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadMask, MonadCatch)

instance Monad m => MonadReader env (LightT env m) where
  ask = LightT ask
  local f = LightT . local f . runLightT'

instance MonadUnliftIO m => MonadUnliftIO (LightT env m) where
  askUnliftIO = LightT $ ReaderT $ \r -> withUnliftIO $ \u -> return (UnliftIO (unliftIO u . flip (runReaderT . runLightT') r))

withResourceMap :: MonadUnliftIO m => (ResourceMap -> m a) -> m a
withResourceMap inner =
  withRunInIO $ \run -> runResourceT $ ResourceT $ run . inner

data LightEnv = LightEnv
  { renderer :: SDL.Renderer
  , resourceMap :: ResourceMap
  }

instance HasLightEnv LightEnv where
  rendererL = lens renderer (\env r -> env { renderer = r })
  resourceMapL = lens resourceMap (\env r -> env { resourceMap = r })

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


