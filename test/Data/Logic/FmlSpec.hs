module Data.Logic.FmlSpec (spec) where

import Data.Logic.Fml
import Data.Logic.Var
import Test.Hspec

vx = Final (mk "x")

vy = Final (mk "y")

vz = Final (mk "z")

va = Final (mk "a")

vb = Final (mk "b")

spec :: Spec
spec = do
  describe "vars" $ do
    it "returns all variables that occur in formula. Duplicate occurrences are removes." $ do
      vars vx `shouldMatchList` [mk "x"]
    it "returns all variables that occur in formula. Duplicate occurrences are removes." $ do
      vars (vx `And` vy) `shouldMatchList` [mk "x", mk "y"]
    it "returns all variables that occur in formula. Duplicate occurrences are removes." $ do
      vars (vx `Or` (vy `And` vz)) `shouldMatchList` [mk "x", mk "y", mk "z"]
    it "returns all variables that occur in formula. Duplicate occurrences are removes." $ do
      vars ((vx `Or` vy) `Or` (va `Or` vb)) `shouldMatchList` [mk "x", mk "y", mk "a", mk "b"]
    it "returns all variables that occur in formula. Duplicate occurrences are removes." $ do
      vars ((vx `Or` vy) `Or` (vx `Or` vy)) `shouldMatchList` [mk "x", mk "y"]

  describe "depth" $ do
    it "returns the depth of formula." $ do
      depth vx `shouldBe` 0
    it "returns the depth of formula." $ do
      depth (Not vx) `shouldBe` 1
    it "returns the depth of formula." $ do
      depth (Not (vx `And` vy)) `shouldBe` 2
    it "returns the depth of formula." $ do
      depth ((vx `Or` vy) `Or` (vy `Or` (vy `Or` vy))) `shouldBe` 3

  describe "toNNF" $ do
    it "NAnd to NNF" $ do
      toNNF (vx `NAnd` vy) `shouldBe` (Not vx `Or` Not vy)
    it "Nor to NNF" $ do
      toNNF (vx `NOr` vy) `shouldBe` (Not vx `And` Not vy)
    it "Xor to NNF" $ do
      toNNF (vx `XOr` vy) `shouldBe` (vx `Or` vy) `And` (Not vx `Or` Not vy)
    it "XNor to NNF" $ do
      toNNF (vx `XNOr` vy) `shouldBe` (vx `And` vy) `Or` (Not vx `And` Not vy)
    it "Imply to NNF" $ do
      toNNF (vx `Imply` vy) `shouldBe` (Not vx `Or` vy)
    it "Equiv to NNF" $ do
      toNNF (vx `Equiv` vy) `shouldBe` (vx `And` vy) `Or` (Not vx `And` Not vy)
    it "Remove double negation" $ do
      toNNF (Not (Not vx)) `shouldBe` vx
    it "Remove double negation 2" $ do
      toNNF (Not (Not (vx `NAnd` vy))) `shouldBe` (Not vx `Or` Not vy)
    it "Remove negation" $ do
      toNNF (Not (vx `NAnd` vy)) `shouldBe` (vx `And` vy)
    it "complexe remove negation" $ do
      toNNF ((vx `NOr` vy) `Imply` (Not vx `And` vy)) `shouldBe` (vx `Or` vy) `Or` (Not vx `And` vy)

  describe "isNNF" $ do
    it "NAnd is not NNF" $ do
      isNNF (vx `NAnd` vy) `shouldBe` False
    it "NNF of NAnd is NNF" $ do
      isNNF (Not vx `Or` Not vy) `shouldBe` True
    it "Nor is not NNF" $ do
      isNNF (vx `NOr` vy) `shouldBe` False
    it "NNF of Nor is NNF" $ do
      isNNF (Not vx `And` Not vy) `shouldBe` True
    it "Xor is not NNF" $ do
      isNNF (vx `XOr` vy) `shouldBe` False
    it "NNF of XOr is NNF" $ do
      isNNF ((vx `Or` vy) `And` (Not vx `Or` Not vy)) `shouldBe` True
    it "XNor is not NNF" $ do
      isNNF (vx `XNOr` vy) `shouldBe` False
    it "NNF of XNor is NNF" $ do
      isNNF ((vx `And` vy) `Or` (Not vx `And` Not vy)) `shouldBe` True
    it "Imply is not NNF" $ do
      isNNF (vx `Imply` vy) `shouldBe` False
    it "NNF of Imply is NNF" $ do
      isNNF (Not vx `Or` vy) `shouldBe` True
    it "Equiv is not NNF" $ do
      isNNF (vx `Equiv` vy) `shouldBe` False
    it "NNF of Equiv is NNF" $ do
      isNNF ((vx `And` vy) `Or` (Not vx `And` Not vy)) `shouldBe` True
    it "Double negation is not NNF" $ do
      isNNF (Not (Not vx)) `shouldBe` False
    it "Negation on formula is not NNF" $ do
      isNNF (Not (And vx vy)) `shouldBe` False
    it "Negation on final is NNF" $ do
      isNNF (Not vx) `shouldBe` True
    it "Complex formula is NNF" $ do
      isNNF ((vx `Or` vy) `Or` (Not vx `And` vy)) `shouldBe` True
    it "Complex formula is not NNF" $ do
      isNNF ((vx `NOr` vy) `Or` (Not vx `And` vy)) `shouldBe` False

  describe "toCNF" $ do
    it "Invert Or and And in simple formula" $ do
      toCNF ((vx `And` Not vy) `Or` (vy `And` vz)) `shouldBe` (((vx `Or` vy) `And` (Not vy `Or` vy)) `And` ((vx `Or` vz) `And` (Not vy `Or` vz)))
    it "Complex toCNF" $ do
      toCNF (((vx `Or` vy) `Or` vz) `Imply` ((vx `Or` vy) `And` (vx `Or` vz))) `shouldBe` ((((Not vx `Or` (vx `Or` vy)) `And` (Not vy `Or` (vx `Or` vy))) `And` (Not vz `Or` (vx `Or` vy))) `And` (((Not vx `Or` (vx `Or` vz)) `And` (Not vy `Or` (vx `Or` vz))) `And` (Not vz `Or` (vx `Or` vz))))

  describe "isCNF" $ do
    it "is CNF simple" $ do
      isCNF (((vx `Or` vy) `And` (Not vy `Or` vy)) `And` ((vx `Or` vz) `And` (Not vy `Or` vz))) `shouldBe` True
    it "is CNF complex" $ do
      isCNF ((((Not vx `Or` (vx `Or` vy)) `And` (Not vy `Or` (vx `Or` vy))) `And` (Not vz `Or` (vx `Or` vy))) `And` (((Not vx `Or` (vx `Or` vz)) `And` (Not vy `Or` (vx `Or` vz))) `And` (Not vz `Or` (vx `Or` vz)))) `shouldBe` True
    it "teacher is CNF" $ do
      isCNF (((vx `Or` Not vy) `And` ((vx `Or` vz) `Or` va)) `And` (Not vx `Or` Not vz)) `shouldBe` True
    it "teacher is not CNF 1" $ do
      isCNF (Not (vx `And` vy)) `shouldBe` False
    it "teacher is not CNF 2" $ do
      isCNF (vx `And` (vy `Or` (vz `And` va))) `shouldBe` False

  describe "toDNF" $ do
    it "toDNF simple" $ do
      toDNF (vx `And` (vy `Or` vz)) `shouldBe` (vy `And` vx) `Or` (vz `And` vx)
    it "complex toDNF" $ do
      toDNF (((vx `Or` vy) `Or` vz) `Imply` ((vx `Or` vy) `And` (vx `Or` vz))) `shouldBe` ((Not vx `And` Not vy) `And` Not vz) `Or` (((vx `And` vx) `Or` (vy `And` vx)) `Or` ((vx `And` vz) `Or` (vy `And` vz)))

  describe "isDNF" $ do
    it "isDNF simple" $ do
      isDNF ((vy `And` vx) `Or` (vz `And` vx)) `shouldBe` True
    it "complex isDNF" $ do
      isDNF (((Not vx `And` Not vy) `And` Not vz) `Or` (((vx `And` vx) `Or` (vy `And` vx)) `Or` ((vx `And` vz) `Or` (vy `And` vz)))) `shouldBe` True
    it "isDNF teacher" $ do
      isDNF (((vx `And` Not vy) `Or` ((vx `And` vz) `And` va)) `Or` (Not vx `And` Not vz)) `shouldBe` True
    it "isNotDNF teacher" $ do
      isDNF (Not (vx `And` vy)) `shouldBe` False
    it "isNotDNF teacher 2" $ do
      isDNF (vx `And` (vy `Or` (vz `And` va))) `shouldBe` False

  describe "toUniversalNAnd" $ do
    it "toUniversalNAnd teacher 1" $ do
      toUniversalNAnd (Not vx) `shouldBe` (vx `NAnd` vx)
    it "toUniversalNAnd teacher 2" $ do
      toUniversalNAnd (vx `Or` vy) `shouldBe` ((vx `NAnd` vx) `NAnd` (vy `NAnd` vy))
    it "toUniversalNAnd teacher 3" $ do
      toUniversalNAnd (vx `And` vy) `shouldBe` ((vx `NAnd` vy) `NAnd` (vx `NAnd` vy))

  describe "toUniversalNor" $ do
    it "toUniversalNor teacher 1" $ do
      toUniversalNOr (Not vx) `shouldBe` (vx `NOr` vx)
    it "toUniversalNor teacher 2" $ do
      toUniversalNOr (vx `Or` vy) `shouldBe` ((vx `NOr` vy) `NOr` (vx `NOr` vy))
    it "toUniversalNor teacher 3" $ do
      toUniversalNOr (vx `And` vy) `shouldBe` ((vx `NOr` vx) `NOr` (vy `NOr` vy))

  describe "toCCNF" $ do
    it "simpleToCCNF" $ do
      toCCNF (((vx `Or` vy) `And` (Not vy `Or` vz)) `And` ((vx `Or` vy) `Or` Not vz)) `shouldBe` ((vx `Or` vy) `And` ((Not vy `Or` vz) `And` ((vx `Or` vy) `Or` Not vz)))

  describe "isCCNF" $ do
    it "isCCNF teacher 1" $ do
      isCCNF (vx `Or` vy) `shouldBe` True
    it "isCCNF teacher 2" $ do
      isCCNF ((vx `Or` vy) `And` (Not vy `Or` vz)) `shouldBe` True
    it "isCCNF teacher 3" $ do
      isCCNF ((vx `Or` vy) `And` ((Not vy `Or` vz) `And` ((vx `Or` vy) `Or` Not vz))) `shouldBe` True
    it "isNotCCNF teacher 1" $ do
      isCCNF (((vx `Or` vy) `And` (Not vy `Or` vz)) `And` ((vx `Or` vy) `Or` Not vz)) `shouldBe` False