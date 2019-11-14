{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Propellant.Propagators.Logic where

import Propellant
import Propellant.Lattices.Evidence
import Algebra.Lattice
import Algebra.Lattice.Wide
import Control.Applicative

(=?) :: forall e a. (Ord e, Eq a) => Cell (Evidence e) (Wide a) -> Cell (Evidence e) (Wide a) -> Cell (Evidence e) (Wide Bool) -> Builder ()
(=?) inA@Cell{} inB@Cell{} out@Cell{} = do
    let p :: Propagator = do
        a <- contents inA
        b <- contents inB
        addContent (liftA2 (liftA2 (==)) a b) out
        equal <- contents out
        let rA :: Evidence e (Wide a) = equal >>~ \case
                        Bottom -> bottom
                        Top -> pure top -- ??
                        Middle True -> a
                        Middle False -> bottom
        let rB :: Evidence e (Wide a) = equal >>~ \case
                        Bottom -> bottom
                        Top -> pure top -- ??
                        Middle True -> b
                        Middle False -> bottom
        addContent rA inB
        addContent rB inA
    addNeighbour inA p
    addNeighbour inB p
