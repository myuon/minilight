{-| The package provides the basics for all components in the library.

A component should have the followings (those can be omitted):

- position: @{x: int, y: int}@
- size: @{width: int, height: int}@
- color: @int[4]@
- font: @{family: string, bold: bool, italic: bool, size: int}@

-}
module Data.Component.Basic where

import Control.Lens hiding (contains)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Typeable
import qualified SDL
import qualified SDL.Vect as Vect
import MiniLight

-- | Basic config type
data Config = Config {
  size :: Vect.V2 Int,
  position :: Vect.V2 Int,
  visible :: Bool
} deriving (Show)

makeClassy_ ''Config

defConfig :: Config
defConfig = Config {size = 0, position = 0, visible = True}

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    sizeMaybe <- v .:? "size"
    size <- (\w -> maybe (return 0) w sizeMaybe) $ withObject "size" $ \v ->
      Vect.V2 <$> v .: "width" <*> v .: "height"

    positionMaybe <- v .:? "position"
    position <- (\w -> maybe (return 0) w positionMaybe) $ withObject "position" $ \v ->
      Vect.V2 <$> v .: "x" <*> v .: "y"

    visibleMaybe <- v .:? "visible"
    let visible = maybe True id visibleMaybe

    return $ Config size position visible

-- | This wrapper function is useful when you write your component config parser.
wrapConfig
  :: (Config -> a -> Parser r) -> (Object -> Parser a) -> Value -> Parser r
wrapConfig f p = withObject "wrapConfig" $ \v -> do
  other <- p v
  conf  <- parseJSON (Object v)
  f conf other

-- | The rectangle region of the component.
areaRectangle :: Config -> SDL.Rectangle Int
areaRectangle conf = SDL.Rectangle (SDL.P (position conf)) (size conf)

-- | Basic signal type.
data Signal where
  MousePressed
    :: Vect.V2 Int  -- ^ The relative position of the mouse pointer
    -> Signal
  MouseReleased
    :: Vect.V2 Int  -- ^ The relative position of the mouse pointer
    -> Signal
  MouseOver
    :: Vect.V2 Int  -- ^ The relative position of the mouse pointer
    -> Signal
  SetVisibility :: Bool -> Signal
  deriving Typeable

instance EventType Signal where
  getEventType (MousePressed _) = "mouse-pressed"
  getEventType (MouseReleased _) = "mouse-released"
  getEventType (MouseOver _) = "mouse-over"
  getEventType (SetVisibility _) = "set-visibility"

  getEventProperties (SetVisibility o) = HM.fromList [("visibility", Bool o)]
  getEventProperties _ = error "not implemeneted yet"

-- | This automatically applies basic configuration such as: position.
wrapFigures :: Config -> [Figure] -> [Figure]
wrapFigures conf fs =
  if not (conf ^. _visible) then [] else map (translate (position conf)) fs

-- | This wrapper function is useful when you write your own 'onSignal' component.
wrapSignal
  :: ( HasLightEnv env
     , HasLoopEnv env
     , HasComponentEnv env
     , MonadIO m
     , ComponentUnit c
     )
  => Lens' c Config  -- ^ lens to 'Config'
  -> (Event -> c -> LightT env m c)  -- ^ custom @onSignal@ function
  -> (Event -> c -> LightT env m c)
wrapSignal lens f ev comp = do
  conf' <- handleBasicSignal ev (comp ^. lens)

  when (comp ^. lens ^. _visible) $ emitBasicSignal ev conf'
  f ev (comp & lens .~ conf')

-- | Basic signaling function. Signals are emitted towards the source component.
emitBasicSignal
  :: (HasLightEnv env, HasLoopEnv env, HasComponentEnv env, MonadIO m)
  => Event
  -> Config
  -> LightT env m ()
emitBasicSignal (RawEvent (SDL.Event _ (SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ (SDL.P pos) _)))) conf
  | contains (areaRectangle conf) (fmap fromEnum pos)
  = view _uid
    >>= \t -> emit (Just t) $ MouseOver $ fmap fromEnum pos - position conf
emitBasicSignal (RawEvent (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ state _ _ _ (SDL.P pos))))) conf
  | contains (areaRectangle conf) (fmap fromEnum pos)
  = view _uid >>= \t ->
    emit (Just t)
      $ ( case state of
          SDL.Pressed  -> MousePressed
          SDL.Released -> MouseReleased
        )
      $ fmap fromEnum pos
      - position conf
emitBasicSignal _ _ = return ()

-- | handle basic signals
handleBasicSignal
  :: (HasLightEnv env, HasLoopEnv env, HasComponentEnv env, MonadIO m)
  => Event
  -> Config
  -> LightT env m Config
handleBasicSignal ev conf = case asSignal ev of
  Just (SetVisibility b) -> return $ conf { visible = b }
  _                      -> return conf

contains :: (Ord a, Num a) => SDL.Rectangle a -> Vect.V2 a -> Bool
contains (SDL.Rectangle (Vect.P (Vect.V2 x y)) (Vect.V2 w h)) (Vect.V2 px py) =
  x <= px && px <= x + w && y <= py && py <= y + h
