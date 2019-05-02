{-| Registry implementation using hashtable
-}
module Data.Registry.HashTable where

import Control.Monad.IO.Class
import qualified Data.HashTable.IO as H
import Data.Foldable (foldlM)
import Data.Maybe (fromJust)
import Data.Registry.Class
import qualified Data.Text as T
import qualified Data.Vector.Mutable.PushBack as VMP

data HashTableImpl k v = HashTableImpl (H.BasicHashTable k Int) (VMP.IOVector v)

instance IRegistry (HashTableImpl T.Text) where
  (!?) (HashTableImpl reg vec) k = liftIO $ do
    i <- H.lookup reg k
    maybe (return Nothing) (fmap Just . VMP.read vec) i

  update (HashTableImpl ht vec) k f = do
    i <- fmap fromJust $ liftIO $ H.lookup ht k
    liftIO (VMP.read vec i) >>= f >>= liftIO . VMP.write vec i

  register (HashTableImpl ht vec) k v = liftIO $ do
    len <- VMP.safeLength vec
    VMP.push vec v
    H.insert ht k len

  asVec (HashTableImpl _ vec) = VMP.asUnsafeIOVector vec

fromListImpl :: MonadIO m => [(T.Text, v)] -> m (HashTableImpl T.Text v)
fromListImpl xs =
  liftIO
    $   HashTableImpl
    <$> ( H.new >>= \h -> foldlM
          (\acc (i, k) -> H.insert acc k i >> return acc)
          h
          (zip [0 ..] $ map fst xs)
        )
    <*> (VMP.fromList $ map snd xs)
