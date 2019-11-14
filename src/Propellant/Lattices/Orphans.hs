{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Propellant.Lattices.Orphans where

import Algebra.Lattice
import Algebra.Lattice.Wide
import Algebra.Lattice.Levitated as L
import Propellant.Lattices.Evidence
import Control.Applicative
import Data.Functor.Compose

instance Num n => Num (Wide n) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional n => Fractional (Wide n) where
  fromRational = pure . fromRational
  recip = fmap recip

instance Num n => Num (Levitated n) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional n => Fractional (Levitated n) where
  fromRational = pure . fromRational
  recip = fmap recip

instance Bounded (Levitated a) where
  minBound = L.Bottom
  maxBound = L.Top


instance Lattice (f (g a)) => Lattice (Compose f g a) where
  Compose a \/ Compose b = Compose (a \/ b)
  Compose a /\ Compose b = Compose (a /\ b)
