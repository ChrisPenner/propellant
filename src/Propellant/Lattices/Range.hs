{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Propellant.Lattices.Range where

import Algebra.Lattice
import Algebra.PartialOrd

data Range n = Range {rangeMin :: n, rangeMax :: n}
  deriving (Show, Eq, Functor)

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

instance Num n => Num (Range n) where
  Range min1 max1 + Range min2 max2 = Range (min1 + min2) (max1 + max2)
  Range min1 max1 - Range min2 max2 = Range (min1 - max2) (max1 - min2)
  Range min1 max1 * Range min2 max2 = Range (min1 * min2) (max1 * max2)
  abs (Range min1 max1) = Range (abs min1) (abs max1)
  signum (Range min1 max1) = Range (signum min1) (signum max1)
  fromInteger n = Range (fromInteger n) (fromInteger n)
instance Fractional n => Fractional (Range n) where
  fromRational r = Range (fromRational r) (fromRational r)
  Range min1 max1 / Range min2 max2 = Range (min1 / max2) (max1 / min2)
