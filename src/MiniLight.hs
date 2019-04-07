module MiniLight (
  module MiniLight.Figure,
  module MiniLight.Light,
  module MiniLight.Component,

  runLightT,
  LoopConfig (..),
  defConfig,
  LoopState (..),
  runMainloop,

  withSDL,
  withWindow
) where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector.Mutable as VM
import Graphics.Text.TrueType
import Lens.Micro.Mtl
import MiniLight.Component
import MiniLight.Light
import MiniLight.Figure
import qualified SDL
import qualified SDL.Font

instance Hashable SDL.Scancode where
  hashWithSalt n sc = hashWithSalt n (SDL.unwrapScancode sc)

runLightT
  :: (HasLightEnv env, MonadIO m, MonadMask m)
  => (LightEnv -> env)
  -> LightT env m a
  -> m a
runLightT init prog = withSDL $ withWindow $ \window -> do
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  fc       <- loadFontCache
  runReaderT (runLightT' prog) $ init $ LightEnv
    { renderer  = renderer
    , fontCache = fc
    }

data LoopConfig = LoopConfig {
  watchKeys :: Maybe [SDL.Scancode],
  appConfigFile :: Maybe FilePath,
  componentResolver :: T.Text -> Aeson.Value -> MiniLight Component
}

defConfig :: LoopConfig
defConfig = LoopConfig
  { watchKeys         = Nothing
  , appConfigFile     = Nothing
  , componentResolver = defResolver
  }

data LoopState = LoopState {
  keyStates :: HM.HashMap SDL.Scancode Int,
  events :: [SDL.Event],
  components :: VM.IOVector Component
}

fromList :: MonadIO m => [a] -> m (VM.IOVector a)
fromList xs = liftIO $ do
  vec <- VM.new $ length xs
  forM_ (zip [0 ..] xs) $ uncurry (VM.write vec)
  return vec

runMainloop
  :: (HasLightEnv env, MonadIO m, MonadMask m)
  => LoopConfig  -- ^ loop config
  -> s  -- ^ initial state
  -> (LoopState -> s -> LightT env m s)  -- ^ loop
  -> LightT env m ()
runMainloop conf initial loop = do
  components <- liftMiniLight $ fromList =<< maybe
    (return [])
    (flip loadAppConfig (componentResolver conf))
    (appConfigFile conf)

  go (LoopState {keyStates = HM.empty, events = [], components = components})
     initial
 where
  go loopState s = do
    renderer <- view rendererL
    liftIO $ SDL.rendererDrawColor renderer SDL.$= 255
    liftIO $ SDL.clear renderer

    forM_ [0 .. VM.length (components loopState) - 1] $ \i -> do
      comp <- liftIO $ VM.read (components loopState) i
      draw comp

    forM_ [0 .. VM.length (components loopState) - 1] $ \i -> do
      comp  <- liftIO $ VM.read (components loopState) i
      comp' <- update comp
      liftIO $ VM.write (components loopState) i comp'

    s' <- loop loopState s

    liftIO $ SDL.present renderer

    liftIO $ threadDelay (100000 `div` 60)
    events <- SDL.pollEvents
    keys   <- SDL.getKeyboardState

    let
      specifiedKeys = HM.mapWithKey
        (\k v -> if keys k then v + 1 else 0)
        ( maybe
            id
            (\specified m -> HM.fromList $ map (\s -> (s, m HM.! s)) specified)
            (watchKeys conf)
        $ keyStates loopState
        )
    let loopState' = loopState { keyStates = specifiedKeys, events = events }
    let quit = any
          ( \event -> case SDL.eventPayload event of
            SDL.WindowClosedEvent _ -> True
            SDL.QuitEvent           -> True
            _                       -> False
          )
          events

    unless quit $ go loopState' s'

--

withSDL :: (MonadIO m, MonadMask m) => m a -> m a
withSDL =
  bracket (SDL.initializeAll >> SDL.Font.initialize)
          (\_ -> SDL.Font.quit >> SDL.quit)
    . const

withWindow :: (MonadIO m, MonadMask m) => (SDL.Window -> m a) -> m a
withWindow =
  bracket (SDL.createWindow "window" SDL.defaultWindow) SDL.destroyWindow
