{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MiniLight.Light where

import Control.Monad.Catch
import Control.Monad.Reader
import Lens.Micro
import qualified SDL

class HasLightEnv env where
  rendererL :: Lens' env SDL.Renderer

newtype LightT env m a = LightT { runLightT' :: ReaderT env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadMask, MonadCatch)

instance Monad m => MonadReader env (LightT env m) where
  ask = LightT ask
  local f = LightT . local f . runLightT'

