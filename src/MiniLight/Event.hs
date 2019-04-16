{-# LANGUAGE ExistentialQuantification #-}
module MiniLight.Event (
  Name,
  Event (..),
  EventType (..),
) where

import qualified SDL
import qualified Data.Text as T
import Language.Haskell.TH (Name)

class EventType b where
  getEventName :: b -> T.Text

data Event
  = Never
  | forall a. EventType a => Signal Name a
  | RawEvent SDL.Event
