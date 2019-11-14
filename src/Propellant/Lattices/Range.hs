{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Propellant.Lattices.Range where

import Algebra.Lattice
import Algebra.PartialOrd

data Range n = Range {rangeMin :: n, rangeMax :: n}
             | InfiniteRange
             | EmptyRange
  deriving (Show, Eq, Functor)

instance Ord n => Lattice (Range n) where
  -- meet
  Range l h /\ Range l' h' = Range (min l l') (max h h')
  InfiniteRange /\ _ = InfiniteRange
  _ /\ InfiniteRange = InfiniteRange
  EmptyRange /\ r = r
  r /\ EmptyRange = r

  -- join
  Range l h \/ Range l' h' | l > h' || h < l' = EmptyRange
  Range l h \/ Range l' h' = Range (max l l') (min h h')
  InfiniteRange \/ r = r
  r \/ InfiniteRange = r
  EmptyRange \/ _ = EmptyRange
  _ \/ EmptyRange = EmptyRange

instance (Ord n) => BoundedJoinSemiLattice (Range n) where
  bottom = EmptyRange

instance (Ord n) => BoundedMeetSemiLattice (Range n) where
  top = InfiniteRange

instance (Eq n, Ord n) => PartialOrd (Range n) where
  leq InfiniteRange _ = True
  leq _ EmptyRange = True
  leq _ InfiniteRange = False
  leq EmptyRange _ = False
  leq (Range l h) (Range l' h')
    | l' >= l && h' <= h = True
    | otherwise = False

collapseRanges :: Range n -> Range n -> Range n
collapseRanges EmptyRange _ = EmptyRange
collapseRanges _ EmptyRange = EmptyRange
collapseRanges InfiniteRange _ = InfiniteRange
collapseRanges _ InfiniteRange = InfiniteRange
collapseRanges _ _ = error "Unhandled special case in collapseRanges"

instance Num n => Num (Range n) where
  Range min1 max1 + Range min2 max2 = Range (min1 + min2) (max1 + max2)
  a + b = collapseRanges a b
  Range min1 max1 - Range min2 max2 = Range (min1 - max2) (max1 - min2)
  a - b = collapseRanges a b
  Range min1 max1 * Range min2 max2 = Range (min1 * min2) (max1 * max2)
  a * b = collapseRanges a b
  abs (Range min1 max1) = Range (abs min1) (abs max1)
  abs x = x
  signum (Range min1 max1) = Range (signum min1) (signum max1)
  signum x = x
  fromInteger n = Range (fromInteger n) (fromInteger n)

instance Fractional n => Fractional (Range n) where
  fromRational r = Range (fromRational r) (fromRational r)
  Range min1 max1 / Range min2 max2 = Range (min1 / max2) (max1 / min2)
  a / b = collapseRanges a b
