module Data.Logic.Fml.Combinator
  ( multOr,
    multAnd,
    allOf,
    noneOf,
    atLeast,
    atLeastOne,
    atMost,
    atMostOne,
    exactly,
    exactlyOne,
  )
where

import Control.Monad
import Data.List
import Data.Logic.Fml
import Data.Logic.Var as Var
import Data.Maybe

-- | ’multOr’ @fs@ returns the disjunction of the formulas in @fs.
--  It returns @Nothing@ if @fs@ is the empty list.
--  >>> Combinator.multOr []
--  Nothing
--  >>> multOr [Fml.Final (Var.mk i) | i <- [1..4]]
--  Just (Or (Final 1) (Or (Final 2) (Or (Final 3) (Final 4))))
multOr :: [Fml a] -> Maybe (Fml a)
multOr [] = Nothing
multOr fmls = Just $ foldr1 Or fmls

-- | ’multAnd’ @fs@ returns the conjunction of the formulas in @fs.
--  It returns @Nothing@ if @fs@ is the empty list.
--  >>> Combinator.multAnd []
--  Nothing
--  multAnd [Fml.Final (Var.mk i) | i <- [1..4]]
-- Just (And (Final 1) (And (Final 2) (And (Final 3) (Final 4))))
multAnd :: [Fml a] -> Maybe (Fml a)
multAnd [] = Nothing
multAnd fmls = Just $ foldr1 And fmls

-- | ’allOf’ @vs@ returns a formula that is satisfiable iff all variables
--  in @vs@ are true. The function returns @Nothing@ if @vs@ is the empty list.
allOf :: [Var.Var a] -> Maybe (Fml a)
allOf = multAnd . map Final

-- | ’noneOf’ @vs@ returns a formula that is satisfiable iff no variable
--  in @vs@ is true. The function returns @Nothing@ if @vs@ is the empty list.
noneOf :: [Var.Var a] -> Maybe (Fml a)
noneOf = multAnd . map (Not . Final)

-- | ’atLeast’ @vs@ @k@ returns a formula that is satisfied iff at least @k@
--  variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
--  empty list or @k@ is non-positive or @k@ is larger than the number of
--  variables in @vs@.
atLeast :: [Var a] -> Int -> Maybe (Fml a)
atLeast [] _ = Nothing
atLeast vars x
  | x <= 0 = Nothing
  | x > length vars = Nothing
  | otherwise = multOr (aux (subsequencesOfSize x vars))
  where
    aux = mapMaybe (multAnd . map Final)

-- Merci SO
subsequencesOfSize :: Int -> [Var a] -> [[Var a]]
-- Reverse here to match with the pdf examples
subsequencesOfSize n xs = reverse $ subsequencesOfSize' n xs
  where
    subsequencesOfSize' n xs =
      let l = length xs
       in if n > l then [] else subsequencesOfSize' xs !! (l - n)
      where
        subsequencesOfSize' [] = [[[]]]
        subsequencesOfSize' (x : xs) =
          let next = subsequencesOfSize' xs
           in zipWith (++) ([] : next) (map (map (x :)) next ++ [[]])

-- | ’atLeastOne’ @vs@ returns a formula that is satisfiable iff at least one
--  variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
--  empty list.
atLeastOne :: [Var.Var a] -> Maybe (Fml a)
-- atLeastOne = flip atLeast 1
atLeastOne l = atLeast l 1

-- | ’atMost’ @vs@ @k@ returns a formula that is satisfiable iff at most @k@
--  variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
--  empty list or @k@ is non-positive or @k@ is larger than the number of
--  variables in @vs@.
atMost :: [Var.Var a] -> Int -> Maybe (Fml a)
atMost [] _ = Nothing
atMost vars x
  | x <= 0 = Nothing
  | x > length vars = Nothing
  | otherwise = multOr (aux (subsequencesOfSize (length vars - x) vars))
  where
    aux = mapMaybe (multAnd . map (Not . Final))

-- | ’atMostOne’ @vs@ returns a formula that is satisfiable iff at most one
--  variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
--  empty list.
atMostOne :: [Var.Var a] -> Maybe (Fml a)
-- atMostOne = flip atMost 1
atMostOne l = atMost l 1

-- | ’exactly’ @vs@ @k@ returns a formula that is satisfiable iff exactly @k@
--  variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
--  empty list or @k@ is non-positive or @k@ is larger than the number of
--  variables in @vs@.
exactly :: [Var.Var a] -> Int -> Maybe (Fml a)
exactly l x = case atLeast l x of
  Nothing -> Nothing
  Just fl -> case atMost l x of
    Nothing -> Nothing
    Just fr -> Just (fl `And` fr)

-- | ’exactlyOne’ @vs@ returns a formula that is satisfiable iff exactly one
--  variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
--  empty list.
exactlyOne :: [Var.Var a] -> Maybe (Fml a)
-- exactlyOne = flip exactly 1
exactlyOne l = exactly l 1
