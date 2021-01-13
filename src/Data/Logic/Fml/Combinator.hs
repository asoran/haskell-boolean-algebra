module Data.Logic.Fml.Combinator
  (
  )
where

import Data.Logic.Fml as Fml
import Data.Logic.Var as Var

-- | ’multOr’ @fs@ returns the disjunction of the formulas in @fs.
--  It returns @Nothing@ if @fs@ is the empty list.
--  >>> Combinator.multOr []
--  Nothing
--  >>> multOr [Fml.Final (Var.mk i) | i <- [1..4]]
--  Just (Or (Final 1) (Or (Final 2) (Or (Final 3) (Final 4))))
multOr :: [Fml.Fml a] -> Maybe (Fml.Fml a)
multOr = Just . head

-- TODO: Cette methode est fausse
