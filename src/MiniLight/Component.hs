module MiniLight.Component (
  HookMap,
  HasComponentEnv(..),
  ComponentEnv(..),
  emit,

  ComponentUnit(..),
  Component,
  _unsafeAs,
  newComponent,
  getComponentSize,
  getUID,
  getHooks,
  setHooks,
  propagate,
) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Text as T
import MiniLight.Light
import MiniLight.Event
import MiniLight.Figure
import qualified SDL
import Unsafe.Coerce

type HookMap = HM.HashMap T.Text (T.Text, Object -> Value)

-- | Environmental information, which are passed for each component
data ComponentEnv = ComponentEnv {
  uid :: T.Text,  -- ^ The unique id
  callbacks :: Maybe HookMap  -- ^ The hooks
}

makeClassy_ ''ComponentEnv

-- | Emit a signal, which will be catched at the next frame.
emit
  :: (HasLoopEnv env, HasComponentEnv env, MonadIO m, EventType et)
  => et
  -> LightT env m ()
emit et = do
  uid <- view _uid
  ref <- view _signalQueue
  liftIO $ modifyIORef' ref $ (signal uid et :)

  hs <- view _callbacks
  case HM.lookup (getEventType et) =<< hs of
    Just (name, param) -> liftIO $ modifyIORef'
      ref
      (signal uid (EventData name (param $ getEventProperties et)) :)
    Nothing -> return ()

-- | CompoonentUnit typeclass provides a way to define a new component.
-- Any 'ComponentUnit' instance can be embedded into 'Component' type.
class ComponentUnit c where
  -- | Updating a model.
  update :: (HasLightEnv env, HasLoopEnv env, HasComponentEnv env, MonadIO m, MonadMask m) => c -> LightT env m c
  update = return

  -- | Descirbes a view. The figures here would be cached. See also 'useCache' for the cache configuration.
  figures :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m [Figure]

  -- | Drawing a figures.
  draw :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> LightT env m ()
  draw comp = liftMiniLight . renders =<< figures comp
  {-# INLINE draw #-}

  -- | Event handlers
  onSignal :: (HasLightEnv env, HasLoopEnv env, HasComponentEnv env, MonadIO m, MonadMask m) => Event -> c -> LightT env m c
  onSignal _ = return

  -- | Return @True@ if a cache stored in the previous frame should be used.
  useCache
    :: c  -- ^ A model value in the previous frame
    -> c  -- ^ A model value in the current frame
    -> Bool
  useCache _ _ = False

  -- | To be called just before clearing caches.
  -- If you want to destroy cached textures for memory efficiency, override this method.
  --
  -- __NB__: Freeing SDL textures and figures are not performed automatically. You must call 'freeFigure' at your own risk.
  beforeClearCache :: (HasLightEnv env, MonadIO m, MonadMask m) => c -> [Figure] -> LightT env m ()
  beforeClearCache _ _ = return ()

-- | A wrapper for 'ComponentUnit' instances.
data Component = forall c. ComponentUnit c => Component {
  uidOf :: T.Text,
  component :: c,
  prev :: c,
  cache :: IORef [Figure],
  callbackObject :: Maybe HookMap
}

-- | Unsafe coercing the component
_unsafeAs :: (ComponentUnit c) => Lens' Component c
_unsafeAs = lens
  (\(Component _ c _ _ _) -> unsafeCoerce c)
  (\(Component a _ c d e) b -> Component a (unsafeCoerce b) c d e)

-- | Create a new component.
newComponent
  :: (ComponentUnit c, HasLightEnv env, MonadIO m, MonadMask m)
  => T.Text
  -> c
  -> LightT env m Component
newComponent uid c = do
  figs <- figures c
  ref  <- liftIO $ newIORef figs
  return $ Component
    { uidOf          = uid
    , component      = c
    , prev           = c
    , cache          = ref
    , callbackObject = Nothing
    }

-- | Get the size of a component.
getComponentSize
  :: (ComponentUnit c, HasLightEnv env, MonadIO m, MonadMask m)
  => c
  -> LightT env m (SDL.Rectangle Int)
getComponentSize comp = do
  figs <- figures comp
  return $ foldl union (SDL.Rectangle (SDL.P 0) 0) $ map targetArea figs

-- | Get its unique id.
getUID :: Component -> T.Text
getUID (Component uid _ _ _ _) = uid

-- | Get the hooks
getHooks :: Component -> Maybe HookMap
getHooks (Component _ _ _ _ h) = h

-- | Get the hooks
setHooks :: Component -> Maybe HookMap -> Component
setHooks (Component uid comp prev cache _) h = Component uid comp prev cache h

-- | Clear the previous model cache and reflect the current model.
propagate :: Component -> Component
propagate (Component uid comp _ cache h) = Component uid comp comp cache h

instance ComponentUnit Component where
  update (Component uid comp prev cache h) = do
    comp' <- update comp
    return $ Component uid comp' prev cache h

  figures (Component _ comp _ _ _) = figures comp

  draw (Component _ comp prev ref _) = do
    if useCache prev comp
      then liftMiniLight . renders =<< liftIO (readIORef ref)
      else do
        figs <- liftIO (readIORef ref)
        beforeClearCache comp figs

        figs <- figures comp
        liftMiniLight $ renders figs
        liftIO $ writeIORef ref figs

  onSignal ev (Component uid comp prev cache h) = fmap (\comp' -> Component uid comp' prev cache h) $ onSignal ev comp
