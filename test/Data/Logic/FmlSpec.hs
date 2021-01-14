module Data.Logic.FmlSpec (spec) where

import Test.Hspec
import Data.Logic.Fml
import Data.Logic.Var

spec :: Spec
spec = do
    describe "vars" $ do
        it "returns all variables that occur in formula. Duplicate occurrences are removes." $ do
            vars (Final (mk "x")) `shouldMatchList` [ mk "x" ]
        it "returns all variables that occur in formula. Duplicate occurrences are removes." $ do
            vars (Final (mk "x") `And` Final (mk "y")) `shouldMatchList` [ mk "x", mk "y" ]
        it "returns all variables that occur in formula. Duplicate occurrences are removes." $ do
            vars (Final (mk "x") `Or` (Final (mk "y") `And` Final (mk "z"))) `shouldMatchList` [ mk "x", mk "y", mk "z" ]
        it "returns all variables that occur in formula. Duplicate occurrences are removes." $ do
            vars ( (Final (mk "x") `Or` Final (mk "y")) `Or` (Final (mk "a") `Or` Final (mk "b")) ) `shouldMatchList` [ mk "x", mk "y", mk "a", mk "b" ]
        it "returns all variables that occur in formula. Duplicate occurrences are removes." $ do
            vars ( (Final (mk "x") `Or` Final (mk "y")) `Or` (Final (mk "x") `Or` Final (mk "y")) ) `shouldMatchList` [ mk "x", mk "y" ]

    describe "depth" $ do
        it "returns the depth of fomula." $ do
            depth (Final (mk "x")) `shouldBe` 0
        it "returns the depth of fomula." $ do
            depth (Not (Final (mk "x"))) `shouldBe` 1
        it "returns the depth of fomula." $ do   
            depth (Not ( Final (mk "x") `And` Final (mk "y") )) `shouldBe` 2
        it "returns the depth of fomula." $ do
            depth ((Final (mk "x") `Or` Final (mk "y")) `Or` (Final (mk "y") `Or` ( Final (mk "y") `Or` Final (mk "y") ))) `shouldBe` 3

    describe "toNNF" $ do
        it "Nand to NNF" $ do
            toNNF (Final (mk "x") `NAnd` Final (mk "y") ) `shouldBe` (Not (Final (mk "x")) `Or` Not (Final (mk "y")) )
        it "Nor to NNF" $ do
            toNNF (Final (mk "x") `NOr` Final (mk "y") ) `shouldBe` (Not (Final (mk "x")) `And` Not (Final (mk "y")) )
        it "Xor to NNF" $ do
            toNNF (Final (mk "x") `XOr` Final (mk "y") ) `shouldBe` (Final (mk "x") `Or` Final (mk "y")) `And` (Not (Final (mk "x")) `Or` Not (Final (mk "y")))
        it "XNor to NNF" $ do
            toNNF (Final (mk "x") `XNOr` Final (mk "y") ) `shouldBe` (Final (mk "x") `And` Final (mk "y")) `Or` (Not (Final (mk "x")) `And` Not (Final (mk "y")))
        it "Imply to NNF" $ do
            toNNF (Final (mk "x") `Imply` Final (mk "y") ) `shouldBe` (Not (Final (mk "x")) `Or` Final (mk "y"))
        it "Equiv to NNF" $ do
            toNNF (Final (mk "x") `Equiv` Final (mk "y") ) `shouldBe` (Final (mk "x") `And` Final (mk "y")) `Or` (Not (Final (mk "x")) `And` Not (Final (mk "y")))
        it "Remove double negation" $ do
            toNNF (Not (Not (Final (mk "x")) )) `shouldBe` Final (mk "x")
        it "Remove double negation 2" $ do
            toNNF (Not (Not ( Final (mk "x") `NAnd` Final (mk "y")))) `shouldBe` (Not (Final (mk "x")) `Or` Not (Final (mk "y")))
        it "Remove negation" $ do
            toNNF (Not (Final (mk "x") `NAnd` Final (mk "y") )) `shouldBe` (Final (mk "x") `And` Final (mk "y"))