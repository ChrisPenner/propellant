{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Propellant.Propagators.Logic where

import Propellant
import Propellant.Propagators as P
import Propellant.Lattices.Evidence
import Algebra.Lattice
import Algebra.Lattice.Wide
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable

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

require :: Cell f (Wide Bool) -> Builder ()
require c@Cell{} = do
    constant' (pure True) c

forbid :: Cell f (Wide Bool) -> Builder ()
forbid c@Cell{} = do
    constant' (pure False) c

distinct :: (Ord e, Eq a, Foldable t) => t (Cell (Evidence e) (Wide a)) -> Builder ()
distinct (toList -> cells) = for_ pairs $ \(a, b) -> do
    x <- emptyCell
    x =! (a =? b)
    forbid x
  where
    pairs = do
        a <- cells
        b <- cells
        guard (a /= b)
        return (a, b)

switch :: forall a e.
       (BoundedLattice a, Ord e)
       => Cell (Evidence e) (Wide Bool)
       -> Cell (Evidence e) a
       -> Cell (Evidence e) a
       -> Builder ()
switch predicateCell inputCell@Cell{} outputCell@Cell{} = do
    let p :: Propagator = do
        shouldPropagate <- contents predicateCell
        input <- contents inputCell
        let rA :: Evidence e a = shouldPropagate >>~ \case
                    Bottom -> bottom
                    Top -> pure top -- ??
                    Middle True -> input
                    Middle False -> bottom
        addContent rA outputCell
    addNeighbour predicateCell p
    addNeighbour inputCell p

notCell :: Cell f (Wide Bool) -> Cell f (Wide Bool) -> Builder ()
notCell inp out = do
    out =! P.map (fmap not) inp

conditional :: forall a e.
            (BoundedLattice a, Ord e)
            => Cell (Evidence e) (Wide Bool)
            -> Cell (Evidence e) a
            -> Cell (Evidence e) a
            -> Cell (Evidence e) a
            -> Builder ()
conditional control onTrue onFalse output = do
    output =! switch control onTrue
    notControl <- store (notCell control)
    output =! switch notControl onFalse

-- oneOf :: forall e t a. (Ord e, Foldable t, Eq a) => t a -> Cell (Evidence e) (Wide a) -> Builder ()
-- oneOf (toList -> values) out = do
--     cells <- for values $ \a -> do
--         newCell $ pure @(Evidence e) (Middle a)
--     oneOfTheCells cells out


-- oneOfTheCells :: Foldable t => t (Cell (Evidence e) a) -> Cell (Evidence e) a -> Builder ()
-- oneOfTheCells = undefined
