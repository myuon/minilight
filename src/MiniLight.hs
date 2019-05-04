{-| MiniLight module exports all basic concepts and oprations except for concrete components.
-}
{-# LANGUAGE FunctionalDependencies #-}
module MiniLight (
  module MiniLight.Light,
  module MiniLight.Event,
  module MiniLight.Figure,
  module MiniLight.Component,

  runLightT,
  LoopState (..),
  LoopConfig (..),
  defConfig,
  runMainloop,
  MiniLoop,
  runMiniloop,
) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import Control.Lens
import qualified Control.Monad.Caster as Caster
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Foldable (foldlM)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.IORef
import qualified Data.Registry as R
import qualified Data.Vector as V
import qualified Data.Text as T
import Graphics.Text.TrueType
import MiniLight.Component
import MiniLight.Event
import MiniLight.Figure
import MiniLight.Light
import qualified System.FSNotify as Notify
import qualified SDL
import qualified SDL.Font

instance Hashable SDL.Scancode where
  hashWithSalt n sc = hashWithSalt n (SDL.unwrapScancode sc)

-- | Run a Light monad.
runLightT :: (MonadIO m, MonadMask m) => LightT LightEnv m a -> m a
runLightT prog = withSDL $ withWindow $ \window -> do
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  fc       <- loadFontCache
  logger   <- liftIO $ Caster.stdoutLogger Caster.LogDebug
  runReaderT (runLightT' prog)
    $ LightEnv {renderer = renderer, fontCache = fc, logger = logger}
 where
  withSDL =
    bracket (SDL.initializeAll >> SDL.Font.initialize)
            (\_ -> SDL.Font.quit >> SDL.quit)
      . const

  withWindow =
    bracket (SDL.createWindow "window" SDL.defaultWindow) SDL.destroyWindow

-- | Use 'defConfig' for a default setting.
data LoopConfig = LoopConfig {
  watchKeys :: Maybe [SDL.Scancode],  -- ^ Set @Nothing@ if all keys should be watched. See also 'LoopState'.
  appConfigFile :: Maybe FilePath,  -- ^ Specify a yaml file which describes component settings. See 'MiniLight.Component' for the yaml syntax.
  hotConfigReplacement :: Maybe FilePath,  -- ^ The directory path to be watched. If set, the config file modification will replace the component dynamically.
  componentResolver :: Resolver,  -- ^ Your custom mappings between a component name and its type.
  additionalComponents :: [Component]  -- ^ The components here would be added during the initialization.
}

-- | Default configurations for the mainloop. You need to set @componentResolver@ if you use a component.
defConfig :: LoopConfig
defConfig = LoopConfig
  { watchKeys            = Nothing
  , appConfigFile        = Nothing
  , hotConfigReplacement = Nothing
  , componentResolver    = \_ _ -> undefined
  , additionalComponents = []
  }

-- | The state in the mainloop.
data LoopState = LoopState {
  light :: LightEnv,
  loop :: LoopEnv,
  loader :: LoaderEnv
}

makeLensesWith classyRules_ ''LoopState

-- | Type synonym to the minimal type of the mainloop
type MiniLoop = LightT LoopState IO


-- These instances are used in the internal computation.
instance HasLightEnv LoopState where
  lightEnv = _light . lightEnv

instance HasLoopEnv LoopState where
  loopEnv = _loop . loopEnv

instance HasLoaderEnv LoopState where
  loaderEnv = _loader . loaderEnv


instance HasLightEnv env' => HasLightEnv (env, env') where
  lightEnv = _2 . lightEnv

instance HasLoopEnv env' => HasLoopEnv (env, env') where
  loopEnv = _2 . loopEnv

instance HasLoaderEnv env' => HasLoaderEnv (env, env') where
  loaderEnv = _2 . loaderEnv


instance HasComponentEnv (T.Text, env) where
  uidL = _1


-- | Same as 'runMainloop' but fixing the type.
runMiniloop :: LoopConfig -> s -> (s -> MiniLoop s) -> MiniLight ()
runMiniloop = runMainloop LoopState

-- | Run a mainloop.
-- In a mainloop, components and events are managed.
--
-- Components in a mainloop: draw ~ update ~ (user-defined function) ~ event handling
runMainloop
  :: ( HasLightEnv env
     , HasLightEnv env'
     , HasLoopEnv env'
     , HasLoaderEnv env'
     , MonadIO m
     , MonadMask m
     )
  => (env -> LoopEnv -> LoaderEnv -> env')  -- ^ Environment conversion
  -> LoopConfig  -- ^ Loop config
  -> s  -- ^ Initial state
  -> (s -> LightT env' m s)  -- ^ A function called in every loop
  -> LightT env m ()
runMainloop conv conf initial userloop = do
  events      <- liftIO $ newMVar []
  signalQueue <- liftIO $ newIORef []
  reg         <- R.new
  conf        <- liftIO $ newIORef $ AppConfig V.empty

  run
    (LoopEnv {keyStates = HM.empty, events = events, signalQueue = signalQueue})
    (LoaderEnv {registry = reg, appConfig = conf})
    initial
 where
  run loop loader s = do
    setup loop loader
    go loop loader s

  setup loop loader = envLightT (\env -> conv env loop loader) $ do
    case (hotConfigReplacement conf, appConfigFile conf) of
      (Just dir, Just confPath) -> do
        liftIO $ forkIO $ Notify.withManager $ \mgr -> do
          _ <- Notify.watchDir mgr dir (const True) $ \ev -> do
            modifyMVar_ (loop ^. _events) $ return . (NotifyEvent ev :)

          forever $ threadDelay 1000000

        loadAppConfig confPath (componentResolver conf)
      _ -> return ()

    forM_ (additionalComponents conf) $ \component -> do
      reg <- view _registry
      R.register reg (getUID component) component

  go loop loader s = do
    renderer <- view _renderer
    liftIO $ SDL.rendererDrawColor renderer SDL.$= 255
    liftIO $ SDL.clear renderer

    R.forV_ (loader ^. _registry) $ \comp -> draw comp

    -- state propagation
    R.modifyV_ (loader ^. _registry) $ return . propagate

    R.modifyV_ (loader ^. _registry) $ \comp ->
      envLightT (\env -> (getUID comp, conv env loop loader)) $ update comp

    s' <- envLightT (\env -> conv env loop loader) $ userloop s

    liftIO $ SDL.present renderer

    liftIO $ threadDelay (100000 `div` 60)
    events <- SDL.pollEvents
    keys   <- SDL.getKeyboardState

    envLightT (\env -> conv env loop loader) $ do
      evref   <- view _events
      sigref  <- view _signalQueue
      signals <- liftIO $ readIORef sigref
      liftIO
        $ modifyMVar_ evref
        $ return
        . (map RawEvent events ++)
        . (signals ++)
      liftIO $ writeIORef sigref []

    envLightT (\env -> conv env loop loader) $ do
      evref  <- view _events
      events <- liftIO $ modifyMVar evref (\a -> return ([], a))

      R.modifyV_ (loader ^. _registry) $ \comp -> do
        foldlM (\comp ev -> envLightT ((,) (getUID comp)) $ onSignal ev comp)
               comp
               events

      forM_
          ( catMaybes $ map
            ( \e -> case e of
              NotifyEvent ev -> Just ev
              _              -> Nothing
            )
            events
          )
        $ \ev -> patchAppConfig
            (fromJust $ appConfigFile conf)
            (componentResolver conf)

    let loop' =
          loop
            &  _keyStates
            %~ HM.mapWithKey (\k v -> if keys k then v + 1 else 0)
            .  maybe
                 id
                 ( \specified m -> HM.fromList $ map
                   (\s -> (s, if HM.member s m then m HM.! s else 0))
                   specified
                 )
                 (watchKeys conf)

    let quit = any
          ( \event -> case SDL.eventPayload event of
            SDL.WindowClosedEvent _ -> True
            SDL.QuitEvent           -> True
            _                       -> False
          )
          events

    unless quit $ go loop' loader s'
