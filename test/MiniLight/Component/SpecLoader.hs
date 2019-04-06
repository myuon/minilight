module MiniLight.Component.SpecLoader where

import MiniLight.Component.Loader
import Test.Tasty.Hspec hiding (Failure, Success)
import Text.Trifecta

spec_parser :: Spec
spec_parser = do
  describe "Parser" $ do
    it "parses a reference" $ do
      case parseString parser mempty "${ref:hoge}" of
        Success _   -> return ()
        Failure err -> expectationFailure $ show err
