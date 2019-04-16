module MiniLight.Event (
  Name,
  Event(..),
) where

import qualified SDL
import qualified Data.Text as T
import Language.Haskell.TH (Name)

data Event
  = Never
  | Signal Name T.Text
  | RawEvent SDL.Event
  deriving (Eq, Show)
