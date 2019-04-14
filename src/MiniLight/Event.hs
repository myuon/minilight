module MiniLight.Event (
  Event(..),
) where

import qualified SDL
import qualified Data.Text as T
import Language.Haskell.TH (Name)

data Event
  = Never
  | ComponentEvent Name T.Text
  | RawEvent SDL.Event
