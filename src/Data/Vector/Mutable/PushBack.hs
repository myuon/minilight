{-| This module provides a variant of the vector, equipped with @push_back@ operation.
IOVector here are supposed to be used in single thread situation.
-}
module Data.Vector.Mutable.PushBack where

import Prelude hiding (length)
import Control.Monad
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import System.IO.Unsafe

-- | @IOVector@ consists of (1) pointer to the underlying vector (2) length
-- While 'Data.Vector' has the underlying array itself, this type only has the pointer.
-- This means read/write should be slower than the original vector.
data IOVector a = IOVector !(IORef (VM.IOVector a)) !(VUM.IOVector Int)

-- Allocate (p + 10)-element vector, which might be more efficient than allocating just a small size of vector, like 1-element or 2-element.
new :: Int -> IO (IOVector a)
new p = new' (p + 10)
  where new' p = IOVector <$> (newIORef =<< VM.new p) <*> (VUM.replicate 1 0)

read :: IOVector a -> Int -> IO a
read (IOVector vref _) k = readIORef vref >>= \vec -> VM.read vec k

-- | Get the position of the last cell in the @IOVector@. This operation is not safe because of the 'unsafePerformIO'.
safeLength :: IOVector a -> IO Int
safeLength (IOVector _ uvec) = VUM.read uvec 0

length :: IOVector a -> Int
length pvec = unsafePerformIO $ safeLength pvec

safeCapacity :: IOVector a -> IO Int
safeCapacity (IOVector vref _) = fmap VM.length $ readIORef vref

-- | Get the capacity of the @IOVector@. This operation is not safe because of the 'unsafePerformIO'.
capacity :: IOVector a -> Int
capacity pvec = unsafePerformIO $ safeCapacity pvec

write :: IOVector a -> Int -> a -> IO ()
write (IOVector vref _) i v = do
  vec <- readIORef vref
  VM.write vec i v

push :: IOVector a -> a -> IO ()
push pvec@(IOVector vref uvec) v = do
  vec <- readIORef vref
  len <- safeLength pvec
  cap <- safeCapacity pvec
  when (len == cap) $ do
    vec' <- VM.grow vec cap
    writeIORef vref vec'

  write      pvec len   v
  VUM.modify uvec (+ 1) 0

fromList :: [a] -> IO (IOVector a)
fromList xs = do
  vec  <- V.thaw $ V.fromList xs
  vref <- newIORef vec
  uvec <- VU.thaw $ VU.fromList [VM.length vec]
  return $ IOVector vref uvec

asIOVector :: IOVector a -> IO (VM.IOVector a)
asIOVector pvec@(IOVector vref _) = do
  len <- safeLength pvec
  readIORef vref >>= \vec -> return (VM.slice 0 len vec)

asUnsafeIOVector :: IOVector a -> VM.IOVector a
asUnsafeIOVector pvec = unsafePerformIO $ asIOVector pvec
