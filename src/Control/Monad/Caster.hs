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
import System.Log.Caster

class MonadLogger m where
  debug :: (MonadIO m, ToBuilder s) => s -> m ()
  info :: (MonadIO m, ToBuilder s) => s -> m ()
  warn :: (MonadIO m, ToBuilder s) => s -> m ()
  err :: (MonadIO m, ToBuilder s) => s -> m ()

stdoutLogger :: LogLevel -> IO LogQueue
stdoutLogger level = do
  chan <- newLogChan
  q    <- newLogQueue

  _    <- forkIO $ relayLog chan level stdoutListener
  _    <- forkIO $ broadcastLog q chan

  return q
