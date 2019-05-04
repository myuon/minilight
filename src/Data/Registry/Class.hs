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

  -- | /O(1)/ Update, raise an exception if the key does not exist.
  update :: MonadIO m => reg v -> T.Text -> (v -> m v) -> m ()
  update reg k f = reg ! k >>= \v -> f v >>= \v' -> write reg k v'

  -- | /O(1)/ Write, raise an exception if the key does not exist.
  write :: MonadIO m => reg v -> T.Text -> v -> m ()

  -- | /O(1)/ Adding a new value to the last position
  register :: MonadIO m => reg v -> T.Text -> v -> m ()

  -- | /O(n)/ Deleting the specified value (this is a slow operation).
  delete :: MonadIO m => reg v -> T.Text -> m ()

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
