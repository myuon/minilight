{-| Registry implementation using hashtable
-}
module Data.Registry.HashTable where

import Control.Monad.IO.Class
import qualified Data.HashTable.IO as H
import Data.List as L
import Data.IORef
import Data.Registry.Class
import qualified Data.Text as T

data HashTableImpl k v = HashTableImpl (IORef [k]) (H.BasicHashTable k v)

instance IRegistry (HashTableImpl T.Text) where
  (!?) (HashTableImpl _ reg) t = liftIO $ H.lookup reg t
  insert (HashTableImpl keys reg) k v = liftIO $ do
    modifyIORef' keys (k:)
    H.insert reg k v
  update (HashTableImpl _ reg) k f = liftIO $ H.mutate reg k (maybe (Nothing, ()) (\v -> (Just $ f v, ())))
  delete (HashTableImpl keys reg) k = liftIO $ do
    modifyIORef' keys (L.delete k)
    H.delete reg k
  keys (HashTableImpl ks _) = liftIO $ fmap reverse $ readIORef ks

newHashTableRegistry :: MonadIO m => m (HashTableImpl T.Text v)
newHashTableRegistry = liftIO $ HashTableImpl <$> newIORef [] <*> H.new
