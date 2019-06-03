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
  = Signal T.Text Dynamic
  | RawEvent SDL.Event
  | NotifyEvent Notify.Event

signal :: EventType a => T.Text -> a -> Event
signal t v = Signal t (toDyn v)

asSignal :: EventType a => Event -> T.Text -> Maybe a
asSignal (Signal t1 v) t2 | t1 == t2 = fromDynamic v
asSignal _             _             = Nothing

-- | Canonical datatype of 'Event'. It consists of event name and event data itself.
data EventData = EventData T.Text Value
  deriving (Show, Typeable)

instance EventType EventData where
  getEventType (EventData t _) = t
  getEventProperties (EventData _ o) = HM.singleton "data" o

asEventData :: Event -> Maybe EventData
asEventData (Signal _ v) = fromDynamic v
asEventData _            = Nothing
