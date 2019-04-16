{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MiniLight.Event (
  Event (..),
  EventType,
  signal,
  asSignal,
) where

import qualified SDL
import qualified Data.Text as T
import Data.Type.Equality
import Type.Reflection

-- | EventType says some type can be used as an event type.
class Typeable e => EventType e

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
  = Never
  | Signal T.Text Dynamic
  | RawEvent SDL.Event

signal :: EventType a => T.Text -> a -> Event
signal t v = Signal t (toDyn v)

asSignal :: EventType a => Event -> T.Text -> Maybe a
asSignal (Signal t1 v) t2 | t1 == t2 = fromDynamic v
asSignal _             _             = Nothing
