module Data.Logic.Fml.CombinatorSpec (spec) where

import Data.Logic.Fml
import Data.Logic.Fml.Combinator
import Data.Logic.Var
import Test.Hspec

v1 :: Fml Integer
v1 = Final (mk 1)

v2 :: Fml Integer
v2 = Final (mk 2)

v3 :: Fml Integer
v3 = Final (mk 3)

v4 :: Fml Integer
v4 = Final (mk 4)

v5 :: Fml Integer
v5 = Final (mk 5)

spec :: Spec
spec = do
  describe "allOf" $ do
    it "allOf" $ do
      allOf [mk i | i <- [1 .. 4]] `shouldBe` Just (And v1 (And v2 (And v3 v4)))
  describe "noneOf" $ do
    it "noneOf" $ do
      noneOf [mk i | i <- [1 .. 4]] `shouldBe` Just (And (Not v1) (And (Not v2) (And (Not v3) (Not v4))))

  describe "atLeast" $ do
    it "atLeast 0 element" $ do
      atLeast [mk i | i <- [1 .. 4]] 0 `shouldBe` Nothing
    it "atLeast 1 element" $ do
      atLeast [mk i | i <- [1 .. 4]] 1 `shouldBe` Just (v1 `Or` (v2 `Or` (v3 `Or` v4)))
    it "atLeast 2 elements" $ do
      atLeast [mk i | i <- [1 .. 4]] 2 `shouldBe` Just ((v1 `And` v2) `Or` ((v1 `And` v3) `Or` ((v1 `And` v4) `Or` ((v2 `And` v3) `Or` ((v2 `And` v4) `Or` (v3 `And` v4))))))

  describe "atLeastOne" $ do
    it "atLeastOne" $ do
      atLeastOne [mk i | i <- [1 .. 4]] `shouldBe` Just (v1 `Or` (v2 `Or` (v3 `Or` v4)))

  describe "atMost" $ do
    it "atMost 0 element" $ do
      atMost [mk i | i <- [1 .. 4]] 0 `shouldBe` Nothing
    it "atMost 1 element" $ do
      atMost [mk i | i <- [1 .. 4]] 1 `shouldBe` Just ((Not v1 `And` (Not v2 `And` Not v3)) `Or` ((Not v1 `And` (Not v2 `And` Not v4)) `Or` ((Not v1 `And` (Not v3 `And` Not v4)) `Or` (Not v2 `And` (Not v3 `And` Not v4)))))
    it "atMost 2 elements" $ do
      atMost [mk i | i <- [1 .. 4]] 2 `shouldBe` Just ((Not v1 `And` Not v2) `Or` ((Not v1 `And` Not v3) `Or` ((Not v1 `And` Not v4) `Or` ((Not v2 `And` Not v3) `Or` ((Not v2 `And` Not v4) `Or` (Not v3 `And` Not v4))))))

  describe "atMostOne" $ do
    it "atMostOne" $ do
      atMostOne [mk i | i <- [1 .. 4]] `shouldBe` Just ((Not v1 `And` (Not v2 `And` Not v3)) `Or` ((Not v1 `And` (Not v2 `And` Not v4)) `Or` ((Not v1 `And` (Not v3 `And` Not v4)) `Or` (Not v2 `And` (Not v3 `And` Not v4)))))

  describe "exactly" $ do
    it "exactly 0 element" $ do
      exactly [mk i | i <- [1 .. 4]] 0 `shouldBe` Nothing
    it "exactly 1 element" $ do
      exactly [mk i | i <- [1 .. 4]] 1 `shouldBe` Just ((v1 `Or` (v2 `Or` (v3 `Or` v4))) `And` ((Not v1 `And` (Not v2 `And` Not v3)) `Or` ((Not v1 `And` (Not v2 `And` Not v4)) `Or` ((Not v1 `And` (Not v3 `And` Not v4)) `Or` (Not v2 `And` (Not v3 `And` Not v4))))))
    it "exactly 2 elements" $ do
      exactly [mk i | i <- [1 .. 4]] 2 `shouldBe` Just (((v1 `And` v2) `Or` ((v1 `And` v3) `Or` ((v1 `And` v4) `Or` ((v2 `And` v3) `Or` ((v2 `And` v4) `Or` (v3 `And` v4)))))) `And` ((Not v1 `And` Not v2) `Or` ((Not v1 `And` Not v3) `Or` ((Not v1 `And` Not v4) `Or` ((Not v2 `And` Not v3) `Or` ((Not v2 `And` Not v4) `Or` (Not v3 `And` Not v4)))))))

  describe "exactlyOne" $ do
    it "exactlyOne" $ do
      exactlyOne [mk i | i <- [1 .. 4]] `shouldBe` Just ((v1 `Or` (v2 `Or` (v3 `Or` v4))) `And` ((Not v1 `And` (Not v2 `And` Not v3)) `Or` ((Not v1 `And` (Not v2 `And` Not v4)) `Or` ((Not v1 `And` (Not v3 `And` Not v4)) `Or` (Not v2 `And` (Not v3 `And` Not v4))))))
