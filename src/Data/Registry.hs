{-| This module provides a registry for the specific type with the Text key.

This module is intended to be imported @qualified@, like:
@
import qualified Data.Registry as R
@
-}
module Data.Registry (
  module Data.Registry.Class,

  Registry(..),
  newRegistry,
) where

import Control.Monad.IO.Class
import Data.Registry.Class
import Data.Registry.HashTable

-- | The @Registry@ type can represents any 'IRegistry' instance.
data Registry v = forall reg. IRegistry reg => Registry (reg v)

instance IRegistry Registry where
  has (Registry reg) t = has reg t
  (!?) (Registry reg) t = (!?) reg t
  insert (Registry reg) k v = insert reg k v
  update (Registry reg) k v = update reg k v
  delete (Registry reg) k = delete reg k
  toList (Registry reg) = toList reg

-- | The current default implementation is using hashtables, defined in the module 'Data.Registry.HashTable'
newRegistry :: MonadIO m => m (Registry v)
newRegistry = fmap Registry newHashTableRegistry
