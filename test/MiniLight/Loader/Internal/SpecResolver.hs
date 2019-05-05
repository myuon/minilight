{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MiniLight.Loader.Internal.SpecResolver where

import Data.Aeson hiding (Success, Failure)
import Data.Yaml.TH
import MiniLight.Loader.Internal.Resolver
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
      case parseString parser mempty "${100}" of
        Success e   -> e `shouldBe` Constant (Number 100)
        Failure err -> expectationFailure $ show err
    it "parses a double" $ do
      case parseString parser mempty "${-0.999}" of
        Success e   -> e `shouldBe` Constant (Number (-0.999))
        Failure err -> expectationFailure $ show err
    it "parses a plus" $ do
      case parseString parser mempty "${10 + 20}" of
        Success e   -> e `shouldBe` Op "+" (Constant (Number 10)) (Constant (Number 20))
        Failure err -> expectationFailure $ show err
    it "parses a times" $ do
      case parseString parser mempty "${10 * 20}" of
        Success e   -> e `shouldBe` Op "*" (Constant (Number 10)) (Constant (Number 20))
        Failure err -> expectationFailure $ show err
    it "parses a complex expression" $ do
      case parseString parser mempty "${${ref:hoge} + 200 * 10 - 100}" of
        Success e -> e `shouldBe` Op
          "-"
          ( Op "+"
               (Ref "hoge")
               (Op "*" (Constant (Number 200)) (Constant (Number 10)))
          )
          (Constant (Number 100))
        Failure err -> expectationFailure $ show err
    it "parses a $-symbol" $ do
      case parseString parser mempty "${$index}" of
        Success e -> e `shouldBe` Symbol "index"
        Failure err -> expectationFailure $ show err
    it "parses a simpl function application" $ do
      case parseString parser mempty "${$func(10)}" of
        Success e -> e `shouldBe` App (Symbol "func") [Constant (Number 10)]
        Failure err -> expectationFailure $ show err
    it "parses a no arg application" $ do
      case parseString parser mempty "${$func()}" of
        Success e -> e `shouldBe` App (Symbol "func") []
        Failure err -> expectationFailure $ show err
    it "parses a function application" $ do
      case parseString parser mempty "${$func(10,$index,${ref:hoge})}" of
        Success e -> e `shouldBe` App (Symbol "func") [Constant (Number 10), Symbol "index", Ref "hoge"]
        Failure err -> expectationFailure $ show err

spec_resolver :: Spec
spec_resolver = do
  describe "Resolver" $ do
    it "resolves a reference, in the same path" $ do
      let plain = [yamlQQ|
        root:
          a: ${ref:b}
          b: 100
      |]
      let expected = [yamlQQ|
        root:
          a: 100
          b: 100
      |]

      resolve plain `shouldBe` expected
    it "resolves a reference, in the same path with period" $ do
      let plain = [yamlQQ|
        root:
          a: ${ref:.b}
          b: 100
      |]
      let expected = [yamlQQ|
        root:
          a: 100
          b: 100
      |]

      resolve plain `shouldBe` expected
    it "resolves a reference, in the parent path" $ do
      let plain = [yamlQQ|
        root:
          child:
            here: ${ref:..target}
          target: 100
      |]
      let expected = [yamlQQ|
        root:
          child:
            here: 100
          target: 100
      |]

      resolve plain `shouldBe` expected
    it "resolves a variable" $ do
      let plain = [yamlQQ|
        root:
          _vars:
            x: 100
          target: ${var:x}
      |]
      let expected = [yamlQQ|
        root:
          target: 100
      |]

      resolve plain `shouldBe` expected
    it "resolves a complex expression" $ do
      let plain = [yamlQQ|
        _vars:
          w: 100
        root:
          h: 200
          ar: ${${var:w} / ${ref:h}}
      |]
      let expected = [yamlQQ|
        root:
          h: 200
          ar: 0.5
      |]

      resolve plain `shouldBe` expected
    it "resolves a nested variable" $ do
      let plain = [yamlQQ|
        _vars:
          nested:
            a: 100
            b: 200
        root:
          c: ${${var:nested.a} + ${var:nested.b}}
      |]
      let expected = [yamlQQ|
        root:
          c: 300
      |]

      resolve plain `shouldBe` expected
    it "resolves a minus expression" $ do
      let plain = [yamlQQ|
        _vars:
          nested:
            a: 200
            b: 150
        root:
          c: ${${var:nested.a} - ${var:nested.b}}
      |]
      let expected = [yamlQQ|
        root:
          c: 50
      |]

      resolve plain `shouldBe` expected
