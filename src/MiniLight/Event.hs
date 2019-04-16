{-# LANGUAGE ExistentialQuantification #-}
module MiniLight.Event (
  Event (..),
  EventType,
) where

import qualified SDL
import qualified Data.Text as T
import Data.Typeable

-- | EventType says some type can be used as an event type.
class Typeable e => EventType e

-- | Event type representation
data Event
  = Never
  | forall a. EventType a => Signal T.Text a
  | RawEvent SDL.Event
