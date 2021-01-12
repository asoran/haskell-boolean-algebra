module Data.Logic.Fml
  ( -- * Type
    Fml (..),

    -- * Querying
    depth,
    vars,

    -- * Formatting
    prettyFormat,

    -- * Transforming
    toNNF,
    toCNF,
    -- toCCNF,
    -- toDNF,
    -- toUniversalNAnd,

    -- * Testing
    isNNF,
    -- isCNF,
    -- isCCNF,
    -- isDNF,
  )
where

import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Logic.Var as Var

data Fml a
  = And (Fml a) (Fml a)
  | NAnd (Fml a) (Fml a)
  | Or (Fml a) (Fml a)
  | NOr (Fml a) (Fml a)
  | XOr (Fml a) (Fml a)
  | XNOr (Fml a) (Fml a)
  | Imply (Fml a) (Fml a)
  | Equiv (Fml a) (Fml a)
  | Not (Fml a)
  | Final (Var.Var a)
  deriving (Show)

prettyFormat :: (Show a) => Fml a -> String
prettyFormat (And p q) = "(" ++ prettyFormat p ++ " . " ++ prettyFormat q ++ ")"
prettyFormat (NAnd p q) = "(" ++ prettyFormat p ++ " ~. " ++ prettyFormat q ++ ")"
prettyFormat (Or p q) = "(" ++ prettyFormat p ++ " + " ++ prettyFormat q ++ ")"
prettyFormat (NOr p q) = "(" ++ prettyFormat p ++ " ~+ " ++ prettyFormat q ++ ")"
prettyFormat (XOr p q) = "(" ++ prettyFormat p ++ " x+ " ++ prettyFormat q ++ ")"
prettyFormat (XNOr p q) = "(" ++ prettyFormat p ++ " x~+ " ++ prettyFormat q ++ ")"
prettyFormat (Imply p q) = "(" ++ prettyFormat p ++ " => " ++ prettyFormat q ++ ")"
prettyFormat (Equiv p q) = "(" ++ prettyFormat p ++ " <=> " ++ prettyFormat q ++ ")"
prettyFormat (Not p) = "-" ++ prettyFormat p
prettyFormat (Final v) = show v

-- | ’vars’ @p@ returns all variables that occur in formula @p@. Duplicate
--  occurrences are removes.
vars :: (Eq a) => Fml a -> [Var.Var a]
vars (Final var) = [var]
vars (Not fml) = vars fml
vars (And fml1 fml2) = vars fml1 ++ vars fml2
vars (NAnd fml1 fml2) = vars fml1 ++ vars fml2
vars (Or fml1 fml2) = vars fml1 ++ vars fml2
vars (NOr fml1 fml2) = vars fml1 ++ vars fml2
vars (XOr fml1 fml2) = vars fml1 ++ vars fml2
vars (XNOr fml1 fml2) = vars fml1 ++ vars fml2
vars (Imply fml1 fml2) = vars fml1 ++ vars fml2
vars (Equiv fml1 fml2) = vars fml1 ++ vars fml2

depth :: (Num b, Ord b) => Fml a -> b
depth (Final _) = 0
depth (Not _) = 1
depth (And fml1 fml2) = 1 + max (depth fml1) (depth fml2)
depth (NAnd fml1 fml2) = 1 + max (depth fml1) (depth fml2)
depth (Or fml1 fml2) = 1 + max (depth fml1) (depth fml2)
depth (NOr fml1 fml2) = 1 + max (depth fml1) (depth fml2)
depth (XOr fml1 fml2) = 1 + max (depth fml1) (depth fml2)
depth (XNOr fml1 fml2) = 1 + max (depth fml1) (depth fml2)
depth (Imply fml1 fml2) = 1 + max (depth fml1) (depth fml2)
depth (Equiv fml1 fml2) = 1 + max (depth fml1) (depth fml2)

-- | ’toNNF’ @f@ converts the formula @f@ to NNF.
toNNF :: Fml a -> Fml a
toNNF (Final var) = Final var
toNNF (Not (Not fml)) = fml
toNNF (Not fml) = Not fml
toNNF (And fml1 fml2) = And fml1 fml2
toNNF (Or fml1 fml2) = Or fml1 fml2
-- oui
toNNF (NAnd fml1 fml2) = Or (Not fml1) (Not fml2)
toNNF (NOr fml1 fml2) = And (Not fml1) (Not fml2)
toNNF (XOr fml1 fml2) = And (Or fml1 fml2) (Or (Not fml1) (Not fml2))
toNNF (XNOr fml1 fml2) = Or (And fml1 fml2) (And (Not fml1) (Not fml2))
toNNF (Imply fml1 fml2) = Or (Not fml1) fml2
toNNF (Equiv fml1 fml2) = Or (And fml1 fml2) (And (Not fml1) (Not fml2))

-- | ’isNNF’ @f@ returns true iff formula @f@ is NNF.
isNNF :: Fml a -> Bool
-- false
isNNF (NAnd _ _) = False
isNNF (NOr _ _) = False
isNNF (XOr _ _) = False
isNNF (XNOr _ _) = False
isNNF (Imply _ _) = False
isNNF (Equiv _ _) = False
-- True
isNNF (Final _) = True
isNNF (Not fml) = isNNF fml
isNNF (Or fml1 fml2) = isNNF fml1 && isNNF fml2
isNNF (And fml1 fml2) = isNNF fml1 && isNNF fml2

-- | ’toCNF’ @f@ converts the formula @f@ to CNF.
toCNF :: Fml a -> Fml a
toCNF fml = nNFtoCNF (toNNF fml)
  where
    nNFtoCNF (Final var) = Final var
    nNFtoCNF (Not fml) = Not (nNFtoCNF fml)
    nNFtoCNF (Or (And fml1 fml2) fml3) = And (Or (nNFtoCNF fml1) (nNFtoCNF fml3)) (Or (nNFtoCNF fml2) (nNFtoCNF fml3))
    nNFtoCNF (Or fml1 (And fml2 fml3)) = And (Or (nNFtoCNF fml1) (nNFtoCNF fml2)) (Or (nNFtoCNF fml1) (nNFtoCNF fml3))
    nNFtoCNF (And fml1 fml2) = And (nNFtoCNF fml1) (nNFtoCNF fml2)
    nNFtoCNF (Or fml1 fml2) = Or (nNFtoCNF fml1) (nNFtoCNF fml2)

-- | ’isCNF’ @f@ returns true iff formula @f@ is CNF.
isCNF :: Fml a -> Bool
-- false
isCNF (NAnd _ _) = False
isCNF (NOr _ _) = False
isCNF (XOr _ _) = False
isCNF (XNOr _ _) = False
isCNF (Imply _ _) = False
isCNF (Equiv _ _) = False
-- true
isCNF (Final _) = True
isCNF (Not fml) = isCNF fml
isCNF (Or (And _ _) _) = False
isCNF (Or _ (And _ _)) = False
isCNF (Or fml1 fml2) = isCNF fml1 && isCNF fml2
isCNF (And fml1 fml2) = isCNF fml1 && isCNF fml2

-- | ’toDNF’ @f@ converts the formula @f@ to DNF.
toDNF :: Fml a -> Fml a
toDNF fml = nNFtoDNF (toNNF fml)
  where
    nNFtoDNF (Final var) = Final var
    nNFtoDNF (Not fml) = Not (nNFtoDNF fml)
    nNFtoDNF (And (Or fml1 fml2) fml3) = Or (And (nNFtoDNF fml1) (nNFtoDNF fml3)) (And (nNFtoDNF fml2) (nNFtoDNF fml3))
    nNFtoDNF (And fml1 (Or fml2 fml3)) = Or (And (nNFtoDNF fml1) (nNFtoDNF fml2)) (And (nNFtoDNF fml1) (nNFtoDNF fml3))
    nNFtoDNF (And fml1 fml2) = And (nNFtoDNF fml1) (nNFtoDNF fml2)
    nNFtoDNF (Or fml1 fml2) = Or (nNFtoDNF fml1) (nNFtoDNF fml2)

-- | ’isDNF’ @f@ returns true iff formula @f@ is DNF.
isDNF :: Fml a -> Bool
-- false
isDNF (NAnd _ _) = False
isDNF (NOr _ _) = False
isDNF (XOr _ _) = False
isDNF (XNOr _ _) = False
isDNF (Imply _ _) = False
isDNF (Equiv _ _) = False
-- true
isDNF (Final _) = True
isDNF (Not fml) = isDNF fml
isDNF (And (Or _ _) _) = False
isDNF (And _ (Or _ _)) = False
isDNF (Or fml1 fml2) = isDNF fml1 && isDNF fml2
isDNF (And fml1 fml2) = isDNF fml1 && isDNF fml2
