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

diff :: AppConfig -> AppConfig -> [(DiffType, Pointer, ComponentConfig)]
diff conf1@(AppConfig comps) conf2 =
  catMaybes $ map go $ Diff.patchOperations $ Diff.diff (toJSON conf1)
                                                        (toJSON conf2)
 where
  toConfig v = (\(Success a) -> a) $ fromJSON v

  go (Add path@(Pointer [OKey "app", AKey _]) v) = Just (New, path, toConfig v)
  go (Rep (Pointer [OKey "app", AKey _, OKey "uid"]) _) = Nothing
  go (Rep (Pointer (OKey "app":AKey n:p)) v) | head p /= OKey "uid" = Just
    ( Modify
    , Pointer [OKey "app", AKey n, OKey "properties"]
    , toConfig $ (\(Success a) -> a) $ Diff.applyOperation
      (Rep (Pointer p) v)
      (toJSON $ comps !! n)
    )
  go (Rem path@(Pointer [OKey "app", AKey n])) =
    Just (Delete, path, toConfig $ toJSON $ comps !! n)
  go (Rem (Pointer (OKey "app":AKey n:p))) = Just
    ( Modify
    , Pointer [OKey "app", AKey n, OKey "properties"]
    , toConfig $ (\(Success a) -> a) $ Diff.applyOperation
      (Rem (Pointer p))
      (toJSON $ comps !! n)
    )
  go op = trace ("unsupported edit: " ++ show op) `seq` Nothing

applyDiff :: [(DiffType, Pointer, ComponentConfig)] -> AppConfig -> AppConfig
applyDiff xs conf =
  (\(Success a) -> a)
    $ fromJSON
    $ (\(Success a) -> a)
    $ (\ys -> Diff.patch (Patch ys) (toJSON conf))
    $ flip map xs
    $ \(typ, path, c) -> case typ of
        Modify -> Rep path (toJSON $ properties c)
        _      -> error "not supported"
