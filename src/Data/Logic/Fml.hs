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
    toCCNF,
    toDNF,
    -- toUniversalNAnd,

    -- * Testing
    isNNF,
    isCNF,
    -- isCCNF,
    isDNF,
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
  deriving (Show, Eq)

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

-- Remove all duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

-- | ’vars’ @p@ returns all variables that occur in formula @p@. Duplicate
--  occurrences are removes.
vars :: (Eq a) => Fml a -> [Var.Var a]
vars = removeDuplicates . vars' where
  vars' :: Fml a -> [Var a]
  vars' (Final var) = [var]
  vars' (Not fml) = vars' fml
  vars' (And fml1 fml2) = vars' fml1 ++ vars' fml2
  vars' (NAnd fml1 fml2) = vars' fml1 ++ vars' fml2
  vars' (Or fml1 fml2) = vars' fml1 ++ vars' fml2
  vars' (NOr fml1 fml2) = vars' fml1 ++ vars' fml2
  vars' (XOr fml1 fml2) = vars' fml1 ++ vars' fml2
  vars' (XNOr fml1 fml2) = vars' fml1 ++ vars' fml2
  vars' (Imply fml1 fml2) = vars' fml1 ++ vars' fml2
  vars' (Equiv fml1 fml2) = vars' fml1 ++ vars' fml2

depth :: (Num b, Ord b) => Fml a -> b
depth (Final _) = 0
depth (Not fml) = 1 + depth fml
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
toNNF (Not (Not fml)) = toNNF fml
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

-- | ’toUniversalNAnd’ @p@ returns a NAND-formula that is equivalent
--  to formula @p@.
toUniversalNAnd :: Fml a -> Fml a
toUniversalNAnd fml = toUniversalNAnd' $ toNNF fml
  where
    toUniversalNAnd' :: Fml a -> Fml a
    toUniversalNAnd' (Final val) = Final val
    toUniversalNAnd' (Not fml) = NAnd computeFml computeFml
      where
        computeFml = toUniversalNAnd' fml
    toUniversalNAnd' (Or fml1 fml2) = NAnd (NAnd computeFml1 computeFml1) (NAnd computeFml2 computeFml2)
      where
        computeFml1 = toUniversalNAnd' fml1
        computeFml2 = toUniversalNAnd' fml2
    toUniversalNAnd' (And fml1 fml2) = NAnd (NAnd computeFml1 computeFml2) (NAnd computeFml1 computeFml2)
      where
        computeFml1 = toUniversalNAnd' fml1
        computeFml2 = toUniversalNAnd' fml2

-- | ’isUniversalNAnd’ @p@ returns true iff formula @p@ uses only NAND
--  and variables.
isUniversalNAnd :: Fml a -> Bool
isUniversalNAnd (Final _) = True
isUniversalNAnd (NAnd fml1 fml2) = isUniversalNAnd fml1 && isUniversalNAnd fml2
isUniversalNAnd _ = False

-- | ’toUniversalNOr’ @p@ returns a NOR-formula that is equivalent
--  to formula @p@.
toUniversalNOr :: Fml a -> Fml a
toUniversalNOr fml = toUniversalNOr' $ toNNF fml
  where
    toUniversalNOr' :: Fml a -> Fml a
    toUniversalNOr' (Final val) = Final val
    toUniversalNOr' (Not fml) = NOr computeFml computeFml
      where
        computeFml = toUniversalNOr' fml
    toUniversalNOr' (Or fml1 fml2) = NOr (NOr computeFml1 computeFml2) (NOr computeFml1 computeFml2)
      where
        computeFml1 = toUniversalNOr' fml1
        computeFml2 = toUniversalNOr' fml2
    toUniversalNOr' (And fml1 fml2) = NOr (NOr computeFml1 computeFml1) (NOr computeFml2 computeFml2)
      where
        computeFml1 = toUniversalNOr' fml1
        computeFml2 = toUniversalNOr' fml2

-- | ’isUniversalNOr’ @p@ returns true iff formula @p@ uses only NOR
--  and variables.
isUniversalNOr :: Fml a -> Bool
isUniversalNOr (Final _) = True
isUniversalNOr (NAnd fml1 fml2) = isUniversalNOr fml1 && isUniversalNOr fml2
isUniversalNOr _ = False

-- | ’toCCNF’ @f@ converts the formula @f@ to CCNF.
toCCNF :: Fml a -> Fml a
-- toCCNF fml = listToCCNF $ cNFtoList $ toCNF fml
toCCNF = listToCCNF . cNFtoList . toCNF
  where
    -- Transforme un CNF en liste de fml pour qu'on lui applique des AND consécutifs
    cNFtoList :: Fml a -> [Fml a]
    cNFtoList (Final val) = [Final val]
    cNFtoList (Or fml1 fml2) = [Or fml1 fml2]
    cNFtoList (And fml1 fml2) = cNFtoList fml1 ++ cNFtoList fml2
    cNFtoList (Not fml) = [Not fml]
    -- Applique des AND consécutifs pour former une CCNF
    listToCCNF :: [Fml a] -> Fml a
    listToCCNF [e] = e
    -- listToCCNF [] = ce cas ne devrais jamais arriver :)
    listToCCNF (fml : rest) = And fml (listToCCNF rest)

-- | ’isCCNF’ @f@ returns true iff formula @f@ is CCNF.
isCCNF :: Fml a -> Bool
isCCNF _ = True

-- TODO: FAIRE
