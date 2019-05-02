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
  MiniLoop,
  runMainloop,
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
data LoopState env = LoopState {
  env :: env,
  loop :: LoopEnv,
  components :: R.Registry Component,
  appConfig :: IORef AppConfig
}

makeLensesWith classyRules_ ''LoopState


instance HasLightEnv env => HasLightEnv (LoopState env) where
  lightEnv = _env . lightEnv

instance HasLoopEnv (LoopState env) where
  loopEnv = _loop . loopEnv

instance HasLightEnv env => HasLightEnv (T.Text, env) where
  lightEnv = _2 . lightEnv

instance HasLoopEnv env => HasLoopEnv (T.Text, env) where
  loopEnv = _2 . loopEnv

instance HasComponentEnv (T.Text, env) where
  uidL = _1

-- | Type synonym to the minimal type of the mainloop
type MiniLoop = LightT (LoopState LightEnv) IO

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
  => (LoopState env -> loop)  -- ^ LoopState conversion function (you can pass @id@, fixing @loop@ as @'LoopState' 'LightEnv'@)
  -> LoopConfig  -- ^ loop config
  -> s  -- ^ initial state
  -> (s -> LightT loop m s)  -- ^ a function called in every loop
  -> LightT env m ()
runMainloop conv conf initial loop = do
  events                <- liftIO $ newMVar []
  signalQueue           <- liftIO $ newIORef []

  (appConfig, compList) <-
    case (hotConfigReplacement conf, appConfigFile conf) of
      (Just dir, Just confPath) -> do
        liftIO $ void $ forkIO $ Notify.withManager $ \mgr -> do
          _ <- Notify.watchDir mgr dir (const True) $ \ev -> do
            modifyMVar_ events $ return . (NotifyEvent ev :)

          forever $ threadDelay 1000000

        liftMiniLight (loadAppConfig confPath (componentResolver conf))
      _ -> return $ (AppConfig [], [])

  reg <- R.fromList
    (map (\c -> (getUID c, c)) $ compList ++ additionalComponents conf)
  config <- liftIO $ newIORef appConfig

  env    <- view id
  go
    ( LoopState
      { env        = env
      , loop       = LoopEnv
        { keyStates   = HM.empty
        , events      = events
        , signalQueue = signalQueue
        }
      , components = reg
      , appConfig  = config
      }
    )
    initial
 where
  go loopState s = do
    renderer <- view _renderer
    liftIO $ SDL.rendererDrawColor renderer SDL.$= 255
    liftIO $ SDL.clear renderer

    R.forV_ (components loopState) $ \comp -> draw comp

    -- state propagation
    R.modifyV_ (components loopState) $ return . propagate

    R.modifyV_ (components loopState) $ \comp ->
      envLightT (\env -> (getUID comp, conv $ loopState { env = env }))
        $ update comp

    s' <- envLightT (\env -> conv $ loopState { env = env }) $ loop s

    liftIO $ SDL.present renderer

    liftIO $ threadDelay (100000 `div` 60)
    events <- SDL.pollEvents
    keys   <- SDL.getKeyboardState

    envLightT (\env -> conv $ loopState { env = env }) $ do
      evref   <- view _events
      sigref  <- view _signalQueue
      signals <- liftIO $ readIORef sigref
      liftIO
        $ modifyMVar_ evref
        $ return
        . (map RawEvent events ++)
        . (signals ++)
      liftIO $ writeIORef sigref []

    envLightT (\env -> conv $ loopState { env = env }) $ do
      evref  <- view _events
      events <- liftIO $ modifyMVar evref (\a -> return ([], a))

      R.modifyV_ (components loopState) $ \comp -> do
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
        $ \ev -> do
            confs0 <- liftIO $ readIORef (appConfig loopState)
            resolveConfig (fromJust $ appConfigFile conf) >>= \case
              Left  err   -> liftIO $ print err
              Right confs -> do
                let d = diff confs0 confs

                forM_ d $ \(typ, _, compConf) -> do
                  liftIO $ print (typ, compConf)
                  case typ of
                    Modify -> do
                      R.update
                        (components loopState)
                        (fromJust $ uid compConf)
                        ( \_ -> liftMiniLight $ createComponentBy
                          (componentResolver conf)
                          compConf
                        )
                    _ -> return ()

                liftIO $ print confs0
                liftIO $ print d
                liftIO $ print (applyDiff d confs0)

                liftIO $ writeIORef (appConfig loopState) $ applyDiff d confs0

    let specifiedKeys = HM.mapWithKey
          (\k v -> if keys k then v + 1 else 0)
          (  maybe
              id
              ( \specified m -> HM.fromList $ map
                (\s -> (s, if HM.member s m then m HM.! s else 0))
                specified
              )
              (watchKeys conf)
          $  loopState
          ^. _keyStates
          )
    let loopState' = loopState & _loop . _keyStates .~ specifiedKeys
    let quit = any
          ( \event -> case SDL.eventPayload event of
            SDL.WindowClosedEvent _ -> True
            SDL.QuitEvent           -> True
            _                       -> False
          )
          events

    unless quit $ go loopState' s'
