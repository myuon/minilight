{-# LANGUAGE OverloadedStrings #-}
module MiniLight.Component.SpecLoader where

import Data.Aeson hiding (Success, Failure)
import MiniLight.Component.Loader
import Test.Tasty.Hspec hiding (Failure, Success)
import Text.Trifecta

spec_parser :: Spec
spec_parser = do
  describe "Trifecta.parsers" $ do
    it "parses a signed double value" $ do
      case parseString integerOrDouble mempty "-0.999" of
        Success e   -> e `shouldBe` Right (-0.999)
        Failure err -> expectationFailure $ show err
  describe "Parser" $ do
    it "parses a reference" $ do
      case parseString parser mempty "${ref:..foo.bar}" of
        Success e   -> e `shouldBe` Ref "..foo.bar"
        Failure err -> expectationFailure $ show err
    it "parses a variable" $ do
      case parseString parser mempty "${var:baz.quux}" of
        Success e   -> e `shouldBe` Var "baz.quux"
        Failure err -> expectationFailure $ show err
    it "parses an integer" $ do
      case parseString parser mempty "{100}" of
        Success e   -> e `shouldBe` Constant (Number 100)
        Failure err -> expectationFailure $ show err
    it "parses a double" $ do
      case parseString parser mempty "{-0.999}" of
        Success e   -> e `shouldBe` Constant (Number (-0.999))
        Failure err -> expectationFailure $ show err
    it "parses a complex expression" $ do
      case parseString parser mempty "{${ref:hoge} + 200 * 10 - 100}" of
        Success e -> e `shouldBe` Op
          "-"
          ( Op "+"
               (Ref "hoge")
               (Op "*" (Constant (Number 200)) (Constant (Number 10)))
          )
          (Constant (Number 100))
        Failure err -> expectationFailure $ show err
