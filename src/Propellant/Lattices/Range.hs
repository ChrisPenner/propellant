{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Propellant.Lattices.Range where

import Propellant.Merge

data Range n = Range {rangeMin :: n, rangeMax :: n}
  deriving (Show, Eq, Functor)

instance Ord n => Mergeable (Range n) where
  r@(Range l h) `merge` r'@(Range l' h')
    | r == r' = NoChange r'
    | h < l' || h' < l = Contradiction
    | otherwise = Changed $ Range (max l l') (min h h')

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
