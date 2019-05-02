{-| A simple logger class for LightT monad -}
module Control.Monad.Caster (
  MonadLogger (..),
  ToBuilder (..),
  LogLevel (..),
  LogQueue,
  stdoutLogger,
) where

import Control.Monad.IO.Class
import Control.Concurrent
import System.Log.Caster as Caster

class MonadLogger m where
  getLogger :: MonadIO m => m LogQueue

  debug :: (MonadIO m, ToBuilder s) => s -> m ()
  debug s = getLogger >>= \g -> Caster.debug g s

  info :: (MonadIO m, ToBuilder s) => s -> m ()
  info s = getLogger >>= \g -> Caster.info g s

  warn :: (MonadIO m, ToBuilder s) => s -> m ()
  warn s = getLogger >>= \g -> Caster.warn g s

  err :: (MonadIO m, ToBuilder s) => s -> m ()
  err s = getLogger >>= \g -> Caster.err g s

stdoutLogger :: LogLevel -> IO LogQueue
stdoutLogger level = do
  chan <- newLogChan
  q    <- newLogQueue

  _    <- forkIO $ relayLog chan level stdoutListener
  _    <- forkIO $ broadcastLog q chan

  return q
