{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
module MiniLight.Light (
  ResourceMap,
  withResourceMap,

  HasLightEnv (..),
  LightT (..),

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

