{-| MiniLight module exports all basic concepts and oprations except for concrete components.
-}
module MiniLight (
  module MiniLight.Light,
  module MiniLight.Event,
  module MiniLight.Figure,
  module MiniLight.Component,

  runLightT,
  LoopConfig (..),
  defConfig,
  LoopEnv (..),
  MiniLoop,
  runMainloop,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Foldable (foldlM)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector.Mutable as VM
import Graphics.Text.TrueType
import Lens.Micro
import Lens.Micro.Mtl
import MiniLight.Component
import MiniLight.Event
import MiniLight.Figure
import MiniLight.Light
import qualified SDL
import qualified SDL.Font

instance Hashable SDL.Scancode where
  hashWithSalt n sc = hashWithSalt n (SDL.unwrapScancode sc)

-- | Run a Light monad.
runLightT :: (MonadIO m, MonadMask m) => LightT LightEnv m a -> m a
runLightT prog = withSDL $ withWindow $ \window -> do
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  fc       <- loadFontCache
  runReaderT (runLightT' prog) $ LightEnv {renderer = renderer, fontCache = fc}

-- | Use 'defConfig' for a default setting.
data LoopConfig = LoopConfig {
  watchKeys :: Maybe [SDL.Scancode],  -- ^ Set @Nothing@ if all keys should be watched. See also 'LoopState'.
  appConfigFile :: Maybe FilePath,  -- ^ Specify a yaml file which describes component settings. See 'MiniLight.Component' for the yaml syntax.
  componentResolver :: T.Text -> Aeson.Value -> MiniLight Component,  -- ^ Your custom mappings between a component name and its type.
  additionalComponents :: [Component]  -- ^ The components here would be added during the initialization.
}

-- | Default configurations for the mainloop.
defConfig :: LoopConfig
defConfig = LoopConfig
  { watchKeys            = Nothing
  , appConfigFile        = Nothing
  , componentResolver    = \_ _ -> undefined
  , additionalComponents = []
  }

fromList :: MonadIO m => [a] -> m (VM.IOVector a)
fromList xs = liftIO $ do
  vec <- VM.new $ length xs
  forM_ (zip [0 ..] xs) $ uncurry (VM.write vec)
  return vec

-- | LoopEnv value would be passed to user side in a mainloop.
data LoopEnv env = LoopState {
  env :: env,
  keyStates :: HM.HashMap SDL.Scancode Int,
  events :: IORef [Event],
  signalQueue :: IORef [Event],
  components :: VM.IOVector Component
}

-- | Lens to the env inside 'LoopState'
envL :: Lens' (LoopEnv env) env
envL = lens env (\e r -> e { env = r })

instance HasLightEnv env => HasLightEnv (LoopEnv env) where
  rendererL = envL . rendererL
  fontCacheL = envL . fontCacheL

instance HasLoopEnv (LoopEnv env) where
  keyStatesL = lens keyStates (\env r -> env { keyStates = r })
  eventsL = lens events (\env r -> env { events = r })
  signalQueueL = lens signalQueue (\env r -> env { signalQueue = r })

instance HasLightEnv env => HasLightEnv (T.Text, env) where
  rendererL = _2 . rendererL
  fontCacheL = _2 . fontCacheL

instance HasLoopEnv env => HasLoopEnv (T.Text, env) where
  keyStatesL = _2 . keyStatesL
  eventsL = _2 . eventsL
  signalQueueL = _2 . signalQueueL

instance HasComponentEnv (T.Text, env) where
  uidL = _1

-- | Type synonym to the minimal type of the mainloop
type MiniLoop = LightT (LoopEnv LightEnv) IO

-- | Run a mainloop.
-- In a mainloop, components and events are managed.
--
-- Components in a mainloop: draw ~ update ~ (user-defined function) ~ event handling
runMainloop
  :: ( HasLightEnv env
     , HasLightEnv loop
     , HasLoopEnv loop
     , MonadIO m
     , MonadMask m
     )
  => (LoopEnv env -> loop)  -- ^ LoopState conversion function (you can pass @id@, fixing @loop@ as @'LoopState' 'LightEnv'@)
  -> LoopConfig  -- ^ loop config
  -> s  -- ^ initial state
  -> (s -> LightT loop m s)  -- ^ a function called in every loop
  -> LightT env m ()
runMainloop conv conf initial loop = do
  components <-
    liftMiniLight $ fromList . (++ additionalComponents conf) =<< maybe
      (return [])
      (flip loadAppConfig (componentResolver conf))
      (appConfigFile conf)
  events      <- liftIO $ newIORef []
  signalQueue <- liftIO $ newIORef []

  env         <- view id
  go
    ( LoopState
      { keyStates   = HM.empty
      , events      = events
      , signalQueue = signalQueue
      , env         = env
      , components  = components
      }
    )
    initial
 where
  go loopState s = do
    renderer <- view rendererL
    liftIO $ SDL.rendererDrawColor renderer SDL.$= 255
    liftIO $ SDL.clear renderer

    forM_ [0 .. VM.length (components loopState) - 1] $ \i -> do
      comp <- liftIO $ VM.read (components loopState) i
      draw comp

    -- state propagation
    forM_ [0 .. VM.length (components loopState) - 1] $ \i -> do
      comp <- liftIO $ VM.read (components loopState) i
      liftIO $ VM.write (components loopState) i (propagate comp)

    forM_ [0 .. VM.length (components loopState) - 1] $ \i -> do
      comp  <- liftIO $ VM.read (components loopState) i
      comp' <- envLightT (\env -> (getUID comp, conv $ loopState { env = env }))
        $ update comp
      liftIO $ VM.write (components loopState) i comp'

    s' <- envLightT (\env -> conv $ loopState { env = env }) $ loop s

    liftIO $ SDL.present renderer

    liftIO $ threadDelay (100000 `div` 60)
    events <- SDL.pollEvents
    keys   <- SDL.getKeyboardState

    envLightT (\env -> conv $ loopState { env = env }) $ do
      evref <- view eventsL
      liftIO $ writeIORef evref $ map RawEvent events

      sigref  <- view signalQueueL
      signals <- liftIO $ readIORef sigref
      liftIO $ modifyIORef evref $ (++ signals)
      liftIO $ writeIORef sigref []

    envLightT (\env -> conv $ loopState { env = env }) $ do
      evref  <- view eventsL
      events <- liftIO $ readIORef evref

      forM_ [0 .. VM.length (components loopState) - 1] $ \i -> do
        comp  <- liftIO $ VM.read (components loopState) i
        comp' <- foldlM
          (\comp ev -> envLightT ((,) (getUID comp)) $ onSignal ev comp)
          comp
          events
        liftIO $ VM.write (components loopState) i comp'

    let
      specifiedKeys = HM.mapWithKey
        (\k v -> if keys k then v + 1 else 0)
        ( maybe
            id
            (\specified m -> HM.fromList $ map (\s -> (s, m HM.! s)) specified)
            (watchKeys conf)
        $ keyStates loopState
        )
    let loopState' = loopState { keyStates = specifiedKeys }
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
