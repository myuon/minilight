{-| This module provides a registry for the specific type with the Text key.

This module is intended to be imported @qualified@, like:
@
import qualified Data.Registry as R
@
-}
module Data.Registry (
  module Data.Registry.Class,

  Registry(..),
  fromList,
) where

import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Registry.Class
import Data.Registry.HashTable

-- | The @Registry@ type can represents any 'IRegistry' instance.
data Registry v = forall reg. IRegistry reg => Registry (reg v)

instance IRegistry Registry where
  (!?) (Registry reg) t = (!?) reg t
  asVec (Registry reg) = asVec reg
--  insert (Registry reg) k v = insert reg k v
--  update (Registry reg) k v = update reg k v
--  delete (Registry reg) k = delete reg k
--  toList (Registry reg) = toList reg

-- | /O(n)/ Create a registry from a list. The current implementation uses a hashtable, defined in the module 'Data.Registry.HashTable'.
fromList :: MonadIO m => [(T.Text, v)] -> m (Registry v)
fromList xs = fmap Registry $ fromListImpl xs
