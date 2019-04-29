module Data.Registry.Class where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Maybe (isJust)

-- | @IRegistry@ typeclass presents a registry interface.
-- The complexity /O(1)/ in the operations can be "amortized" complexity.
class IRegistry reg where
  -- | /O(1)/ Checking if the specified key exists
  has :: MonadIO m => reg v -> T.Text -> m Bool
  has reg k = fmap isJust $ reg !? k

  -- | /O(1)/ Indexing
  (!) :: MonadIO m => reg v -> T.Text -> m v
  reg ! k = fmap (\(Just a) -> a) $ reg !? k

  -- | /O(1)/ Safe indexing
  (!?) :: MonadIO m => reg v -> T.Text -> m (Maybe v)

  -- | /O(1)/ Insert
  insert :: MonadIO m => reg v -> T.Text -> v -> m ()

  -- | /O(1)/ Update
  update :: MonadIO m => reg v -> T.Text -> (v -> v) -> m ()

  -- | /O(1)/ Delete
  delete :: MonadIO m => reg v -> T.Text -> m ()

  -- | /O(n)/ Get the keys. (Should preserve the order)
  keys :: MonadIO m => reg v -> m [T.Text]
  keys reg = fmap (map fst) $ toList reg

  -- | /O(n)/ Convert the registry to the lazy list.
  toList :: MonadIO m => reg v -> m [(T.Text, v)]
  toList reg = keys reg >>= mapM (\k -> fmap ((,) k) $ reg ! k)

  -- | /O(n)/ Iterative loop over the items.
  iforM_ :: MonadIO m => reg v -> (T.Text -> v -> m ()) -> m ()
  iforM_ reg f = do
    xs <- toList reg
    forM_ xs $ \(k,v) -> f k v

infixl 9 !
infixl 9 !?
