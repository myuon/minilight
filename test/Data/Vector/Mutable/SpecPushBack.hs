module Data.Vector.Mutable.SpecPushBack where

import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable.PushBack as VMP
import Test.Tasty.Hspec hiding (Failure, Success)

spec_pushback :: Spec
spec_pushback = do
  describe "IOVector" $ do
    it "can write and read" $ do
      let message = "hello"
      vec <- VMP.new 1
      VMP.write vec 0 message

      actual <- VMP.read vec 0
      message `shouldBe` actual

    it "can get the current length and the capacity" $ do
      vec <- VMP.fromList [1, 2, 3, 4, 5]
      VMP.length vec `shouldBe` 5
      VMP.capacity vec `shouldSatisfy` (>= 5)

    it "can push_back and read" $ do
      let message = "hello"
      vec <- VMP.new 0
      VMP.push vec message

      actual <- VMP.read vec 0
      actual `shouldBe` message

    it "can push_back many times" $ do
      vec <- VMP.new 0
      forM_ [0 .. 1000] $ VMP.push vec

      actual <- VMP.read vec 999
      actual `shouldBe` 999

    it "can insert a value" $ do
      vec <- VMP.fromList [1 .. 5]
      VMP.insert vec 3 999

      actual <- V.freeze =<< VMP.asIOVector vec
      actual `shouldBe` V.fromList [1, 2, 3, 999, 4, 5]

    it "can delete a value" $ do
      vec <- VMP.fromList [1 .. 5]
      VMP.delete vec 3

      actual <- V.freeze =<< VMP.asIOVector vec
      actual `shouldBe` V.fromList [1, 2, 3, 5]

    it "can insert a value to the last" $ do
      vec <- VMP.fromList [1 .. 5]
      VMP.insert vec 5 999

      actual <- V.freeze =<< VMP.asIOVector vec
      actual `shouldBe` V.fromList [1, 2, 3, 4, 5, 999]
