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
        