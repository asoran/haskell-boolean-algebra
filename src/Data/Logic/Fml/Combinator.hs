module Data.Logic.Fml.Combinator
  ( -- * Type
    Var (..),

    -- * Constructing
    mk,
  )
where

-- | 'Combinator' type
newtype Var a = Combinator {truc :: a} deriving (Eq, Ord)

mk n = Combinator {truc = n}
