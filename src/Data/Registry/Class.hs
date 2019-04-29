module Data.Registry.Class where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Vector.Mutable as VM

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

--  -- | /O(1)/ Update, raise an exception if the key does not exists.
--  update :: MonadIO m => reg v -> T.Text -> (v -> m v) -> m ()

--  -- | /O(n)/ Delete, the complexity would be reduced in the future.
--  delete :: MonadIO m => reg v -> T.Text -> m ()

--  -- | /O(n)/ Get the keys, should preserve the order (FIFO).
--  keys :: MonadIO m => reg v -> m [T.Text]
--  keys reg = fmap (map fst) $ toList reg

--  -- | /O(n)/ Convert the registry to the lazy list.
--  toList :: MonadIO m => reg v -> m [(T.Text, v)]
--  toList reg = keys reg >>= mapM (\k -> fmap ((,) k) $ reg ! k)

  -- | /O(1)/ Get the underlying vector. Be careful: modifying the vector might cause a problem.
  asVec :: reg v -> VM.IOVector v

infixl 9 !
infixl 9 !?

-- | For-loop over the registry, ignoring the key order.
forV_ :: (MonadIO m, IRegistry reg) => reg v -> (v -> m ()) -> m ()
forV_ reg iter =
  let vec = asVec reg
  in  forM_ [0 .. VM.length vec - 1] $ \i -> liftIO (VM.read vec i) >>= iter

-- | For-loop over the registry with the index, ignoring the key order.
iforV_ :: (MonadIO m, IRegistry reg) => reg v -> (Int -> v -> m ()) -> m ()
iforV_ reg iter =
  let vec = asVec reg
  in  forM_ [0 .. VM.length vec - 1] $ \i -> liftIO (VM.read vec i) >>= iter i

-- | Modifying the item one by one, ignoring the key order.
modifyV_ :: (MonadIO m, IRegistry reg) => reg v -> (v -> m v) -> m ()
modifyV_ reg iter = iforV_ reg $ \i v -> do
  v' <- iter v
  liftIO $ VM.write (asVec reg) i v'
