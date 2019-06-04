{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MiniLight.Event (
  Event (..),
  EventType (..),
  EventData (..),
  signal,
  asSignal,
  asRawEvent,
  asNotifyEvent,
  asEventData,
) where

import qualified SDL
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Type.Equality
import Type.Reflection
import qualified System.FSNotify as Notify

-- | EventType says some type can be used as an event type.
class Typeable e => EventType e where
  getEventType :: e -> T.Text
  default getEventType :: Show e => e -> T.Text
  getEventType = T.pack . show

  getEventProperties :: e -> Object
  getEventProperties _ = HM.empty

-- | This type is same as 'Dynamic' from @Data.Dynamic@, but it requires 'EventType' contraint.
data Dynamic where
  Dynamic :: forall a. EventType a => TypeRep a -> a -> Dynamic

toDyn :: EventType a => a -> Dynamic
toDyn v = Dynamic typeRep v

fromDynamic :: forall a . (EventType a) => Dynamic -> Maybe a
fromDynamic (Dynamic t v) | Just HRefl <- t `eqTypeRep` rep = Just v
                          | otherwise                       = Nothing
  where rep = typeRep :: TypeRep a

-- | Event type representation
data Event
  = Signal T.Text (Maybe T.Text) Dynamic
  | RawEvent SDL.Event
  | NotifyEvent Notify.Event

-- | Create a signal event.
signal
  :: EventType a
  => T.Text  -- ^ source component ID
  -> Maybe T.Text  -- ^ target component ID, leave Nothing if this is a global event
  -> a
  -> Event
signal s t v = Signal s t (toDyn v)

-- | Cast a signal event to some 'EventType'.
asSignal :: EventType a => Event -> Maybe a
asSignal (Signal _ _ v) = fromDynamic v
asSignal _              = Nothing

-- | Cast an event to some 'SDL.Event'
asRawEvent :: Event -> Maybe SDL.Event
asRawEvent (RawEvent e) = Just e
asRawEvent _            = Nothing

-- | Cast an event to some 'Notify.Event'
asNotifyEvent :: Event -> Maybe Notify.Event
asNotifyEvent (NotifyEvent e) = Just e
asNotifyEvent _               = Nothing

-- | Canonical datatype of 'Event'. It consists of event name and event data itself.
-- This type is usually used for global events.
data EventData = EventData T.Text Value
  deriving (Show, Typeable)

instance EventType EventData where
  getEventType (EventData t _) = t
  getEventProperties (EventData _ o) = HM.singleton "data" o

-- | Cast a signal event to 'EventData'
asEventData :: Event -> Maybe EventData
asEventData (Signal _ Nothing v) = fromDynamic v
asEventData _                    = Nothing
