module Data.Logic.Fml.Some
  (
  )
where

import Data.Logic.Fml
import qualified Data.Logic.Fml as Fml
import qualified Data.Logic.Var as Var
import Data.Logic.Var.Some
import qualified Data.Logic.Var.Some as Var.Some

vx = Final $ Var.mk "x"

vy = Final $ Var.mk "y"

vz = Final $ Var.mk "z"

-- toNNF

toNnfImply = Imply vx vy

-- TODO: ????
-- fml1 :: Fml.Fml String
-- fml1 = Fml.Final Var.Some.x1

-- -- | Formula #14
-- --
-- --  \[
-- --  ((x_1 \uparrow \neg x_2) \Leftrightarrow (x_3 \oplus x_4))
-- --  \Rightarrow
-- --  \neg ((x_5 \vee x_6) \downarrow (x_7 \Leftrightarrow \neg x_8))
-- --  \]
-- fml14 :: Fml.Fml String
-- fml14 = f
--   where
--     f = g `Fml.Imply` h
--     g = g1 `Fml.Equiv` g2
--     g1 = x1 `Fml.NAnd` Fml.Not x2
--     g2 = x3 `Fml.XOr` x4
--     h = Fml.Not (h1 `Fml.NOr` h2)
--     h1 = x5 `Fml.Or` x6
--     h2 = x7 `Fml.Equiv` Fml.Not x8