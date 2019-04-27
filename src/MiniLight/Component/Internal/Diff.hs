module MiniLight.Component.Internal.Diff where

import Data.Aeson
import qualified Data.Aeson.Diff as Diff
import MiniLight.Component.Internal.Types

data DiffType
  = New
  | Modify
  | Delete

diff :: AppConfig -> AppConfig -> [Diff.Operation]
diff conf1 conf2 =
  Diff.patchOperations $ Diff.diff (toJSON conf1) (toJSON conf2)
