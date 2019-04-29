{-| Registry implementation using hashtable
-}
module Data.Registry.HashTable where

import Control.Monad.IO.Class
import qualified Data.HashTable.IO as H
import Data.Registry.Class
import qualified Data.Text as T

newtype HashTableImpl k v = HashTableImpl (H.BasicHashTable k v)

instance IRegistry (HashTableImpl T.Text) where
  (!?) (HashTableImpl reg) t = liftIO $ H.lookup reg t
  insert (HashTableImpl reg) k v = liftIO $ H.insert reg k v
  update (HashTableImpl reg) k f = liftIO $ H.mutate reg k (maybe (Nothing, ()) (\v -> (Just $ f v, ())))
  delete (HashTableImpl reg) k = liftIO $ H.delete reg k
  toList (HashTableImpl reg) = liftIO $ H.toList reg

newHashTableRegistry :: MonadIO m => m (HashTableImpl T.Text v)
newHashTableRegistry = liftIO $ fmap HashTableImpl $ H.new
