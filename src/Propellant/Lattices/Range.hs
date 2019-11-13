module Propellant.Lattices.Range where

import Algebra.Lattice

data Range n = Range {rangeMin :: n, rangeMax :: n}
  deriving (Show, Eq)

instance Ord n => Lattice (Range n) where
  -- meet
  Range l h /\ Range l' h' = Range (min l l') (max h h')
  -- join
  Range l h \/ Range l' h' = Range (max l l') (min h h')

instance (Ord n, Bounded n) => BoundedJoinSemiLattice (Range n) where
  bottom = Range minBound maxBound
