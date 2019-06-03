{-| The package provides the basics for all components in the library.

A component should have the followings (those can be omitted):

- position: @{x: int, y: int}@
- size: @{width: int, height: int}@
- color: @int[4]@
- font: @{family: string, bold: bool, italic: bool, size: int}@

-}
module Data.Component.Basic where

import Control.Lens hiding (contains)
import Data.Aeson
import Data.Aeson.Types
import Data.Typeable
import qualified SDL
import qualified SDL.Vect as Vect
import MiniLight

-- | Basic config type
data Config = Config {
  size :: Vect.V2 Int,
  position :: Vect.V2 Int,
  disabled :: Bool
} deriving (Show)

makeClassy_ ''Config

defConfig :: Config
defConfig = Config {size = 0, position = 0, disabled = False}

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    sizeMaybe <- v .:? "size"
    size <- (\w -> maybe (return 0) w sizeMaybe) $ withObject "size" $ \v ->
      Vect.V2 <$> v .: "width" <*> v .: "height"

    positionMaybe <- v .:? "position"
    position <- (\w -> maybe (return 0) w positionMaybe) $ withObject "position" $ \v ->
      Vect.V2 <$> v .: "x" <*> v .: "y"

    disabledMaybe <- v .:? "disabled"
    let disabled = maybe False id disabledMaybe

    return $ Config size position disabled

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
  deriving Typeable

instance EventType Signal where
  getEventType (MousePressed _) = "mouse-pressed"
  getEventType (MouseReleased _) = "mouse-released"
  getEventType (MouseOver _) = "mouse-over"

-- | This automatically applies basic configuration such as: position.
wrapFigures :: Config -> [Figure] -> [Figure]
wrapFigures conf fs =
  if disabled conf then [] else map (translate (position conf)) fs

-- | This wrapper function is useful when you write your own 'onSignal' component.
wrapSignal
  :: ( HasLightEnv env
     , HasLoopEnv env
     , HasComponentEnv env
     , MonadIO m
     , ComponentUnit c
     )
  => (c -> Config)  -- ^ 'Config' getter
  -> (Event -> c -> LightT env m c)  -- ^ Custom @onSignal@ function
  -> (Event -> c -> LightT env m c)
wrapSignal getter f ev comp = if disabled (getter comp)
  then return comp
  else do
    emitBasicSignal ev (getter comp)
    f               ev comp

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

-- | Disable the component, no drawing and no event handling (update might be working though)
disable :: Config -> Config
disable conf = conf { disabled = True }

-- | Enable the component
enable :: Config -> Config
enable conf = conf { disabled = False }

contains :: (Ord a, Num a) => SDL.Rectangle a -> Vect.V2 a -> Bool
contains (SDL.Rectangle (Vect.P (Vect.V2 x y)) (Vect.V2 w h)) (Vect.V2 px py) =
  x <= px && px <= x + w && y <= py && py <= y + h
