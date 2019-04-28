module MiniLight.Component.Internal.Diff where

import Data.Aeson
import qualified Data.Aeson.Diff as Diff
import Data.Aeson.Pointer
import Data.Aeson.Patch
import Data.Maybe (catMaybes)
import MiniLight.Component.Internal.Types
import Debug.Trace

data DiffType
  = New
  | Modify
  | Delete
  deriving (Eq, Show)

diff :: AppConfig -> AppConfig -> [(DiffType, Value)]
diff conf1@(AppConfig comps) conf2 =
  catMaybes $ map go $ Diff.patchOperations $ Diff.diff (toJSON conf1)
                                                        (toJSON conf2)
 where
  go (Add (Pointer [OKey "app", AKey _]) v) = Just (New, v)
  go (Rep (Pointer (OKey "app":AKey n:path)) v) | head path /= OKey "uid" = Just
    ( Modify
    , (\(Success a) -> a)
      $ Diff.applyOperation (Rep (Pointer path) v) (toJSON $ comps !! n)
    )
  go (Rep (Pointer (OKey "app":AKey n:path)) v) | otherwise = Nothing
  go (Rem (Pointer [OKey "app", AKey n])) = Just (Delete, toJSON $ comps !! n)
  go (Rem (Pointer (OKey "app":AKey n:path))) = Just
    ( Modify
    , (\(Success a) -> a)
      $ Diff.applyOperation (Rem (Pointer path)) (toJSON $ comps !! n)
    )
  go op = trace ("unsupported edit: " ++ show op) `seq` Nothing
