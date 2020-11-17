module Data.Logic.Var
  ( -- * Type
    Var (..),

    -- * Constructing
    mk,
  )
where

-- | 'Var' type
newtype Var a = Var {getName :: a} deriving (Eq, Ord)

-- | Show instance
instance (Show a) => Show (Var a) where
  show = show . getName

-- | ’mk’ @n@ makes a propositional variable with name @n@.
--
--  >>> [mk i | i <- [1..4]]
--  [1,2,3,4]
--  >>> [mk ("x" ++ show i) | i <- [1..4]]
--  ["x1","x2","x3","x4"]
mk :: a -> Var a
mk n = Var {getName = n}
