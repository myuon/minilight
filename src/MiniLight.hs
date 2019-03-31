module MiniLight (
  module MiniLight.Figure,
  module MiniLight.Light,
  module MiniLight.Layers,
  module MiniLight.Component,

  runLightT,
  LoopConfig (..),
  LoopState (..),
  runMainloop,

  withSDL,
  withWindow,
  withFont
) where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Unlift
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Mtl
import MiniLight.Component
import MiniLight.Light
import MiniLight.Figure
import MiniLight.Layers
import qualified SDL
import qualified SDL.Font
import qualified SDL.Vect as Vect

runLightT
  :: (HasLightEnv env, MonadIO m, MonadMask m, MonadUnliftIO m)
  => (LightEnv -> env)
  -> LightT env m a
  -> m a
runLightT init prog = withSDL $ withWindow $ \window -> do
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  withResourceMap $ \rmap -> runReaderT (runLightT' prog) $ init $ LightEnv
    { renderer    = renderer
    , resourceMap = rmap
    }

data LoopConfig = LoopConfig {
  watchKeys :: Maybe [SDL.Scancode]
}

data LoopState = LoopState {
  keyStates :: M.Map SDL.Scancode Int,
  events :: [SDL.Event]
}

runMainloop
  :: (HasLightEnv env, MonadIO m)
  => LoopConfig  -- ^ loop config
  -> s  -- ^ initial state
  -> (LoopState -> s -> LightT env m s)  -- ^ loop
  -> LightT env m ()
runMainloop conf initial loop = go
  (LoopState {keyStates = M.empty, events = []})
  initial
 where
  go loopState s = do
    renderer <- view rendererL
    liftIO $ SDL.rendererDrawColor renderer SDL.$= 255
    liftIO $ SDL.clear renderer

    s' <- loop loopState s

    liftIO $ SDL.present renderer

    liftIO $ threadDelay (100000 `div` 60)
    events <- SDL.pollEvents
    keys   <- SDL.getKeyboardState

    let
      specifiedKeys = M.mapWithKey
        (\k v -> if keys k then v + 1 else 0)
        ( maybe
            id
            (\specified m -> M.fromList $ map (\s -> (s, m M.! s)) specified)
            (watchKeys conf)
        $ keyStates loopState
        )
    let loopState = LoopState {keyStates = specifiedKeys, events = events}
    let quit = any
          ( \event -> case SDL.eventPayload event of
            SDL.WindowClosedEvent _ -> True
            SDL.QuitEvent           -> True
            _                       -> False
          )
          events

    unless quit $ go loopState s'

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

