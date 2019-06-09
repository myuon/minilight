module Main.SpecMain where

import MiniLight
import Test.Tasty.Hspec hiding (Failure, Success)

spec_main :: Spec
spec_main = do
  describe "main" $ do
    it "should start and quit" $ do
      () <- runLightTWith (defLightConfig { headless = True }) $ do
        runMiniloop defConfig () (\_ -> quit)
      () `shouldBe` ()
