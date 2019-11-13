module Propellant.Lattices.Range where

import Algebra.Lattice
import Algebra.PartialOrd

data Range n = Range {rangeMin :: n, rangeMax :: n}
  deriving (Show, Eq)

instance Ord n => Lattice (Range n) where
  -- meet
  Range l h /\ Range l' h' = Range (min l l') (max h h')
  -- join
  Range l h \/ Range l' h' = Range (max l l') (min h h')

instance (Ord n, Bounded n) => BoundedJoinSemiLattice (Range n) where
  bottom = Range minBound maxBound

instance (Ord n, Bounded n) => BoundedMeetSemiLattice (Range n) where
  top = Range maxBound minBound


instance (Eq n, Ord n) => PartialOrd (Range n) where
  leq (Range l h) (Range l' h')
    | l' >= l && h' <= h = True
    | otherwise = False
