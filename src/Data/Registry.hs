{-| This module provides a registry for the specific type with the Text key.

This module is intended to be imported @qualified@, like:
@
import qualified Data.Registry as R
@
-}
module Data.Registry where

import Control.Monad.IO.Class
import qualified Data.Text as T

-- | @IRegistry@ typeclass presents a registry interface.
-- The complexity /O(1)/ in the operations are "amortized" complexity.
class IRegistry reg where
  -- | /O(1)/ Checking if the specified key exists
  has :: MonadIO m => reg v -> T.Text -> m Bool

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

infixl 9 !
infixl 9 !?

data Registry v = forall reg. IRegistry reg => Registry (reg v)

instance IRegistry Registry where
  has (Registry reg) t = has reg t
  (!?) (Registry reg) t = (!?) reg t
  insert (Registry reg) k v = insert reg k v
  update (Registry reg) k v = update reg k v
  delete (Registry reg) k = delete reg k
