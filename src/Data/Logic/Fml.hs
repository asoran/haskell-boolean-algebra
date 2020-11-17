module Data.Logic.Fml
  ( -- * Type
    Fml (..),

    -- * Querying

    -- depth,
    vars,

    -- * Formatting
    prettyFormat,

    -- * Transforming

    -- toNNF,
    -- toCNF,
    -- toCCNF,
    -- toDNF,
    -- toUniversalNAnd,

    -- * Testing

    -- isNNF,
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