{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Propellant.Lattices.Num where

import Algebra.Lattice.Wide
import Algebra.Lattice.Levitated as L
import Control.Applicative

instance Num n => Num (Wide n) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional (Wide Rational) where
  fromRational = pure . fromRational
  recip = fmap recip

instance Num n => Num (Levitated n) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional (Levitated Rational) where
  fromRational = pure . fromRational
  recip = fmap recip

instance Bounded (Levitated a) where
  minBound = L.Bottom
  maxBound = L.Top
