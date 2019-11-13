{-# OPTIONS_GHC -fno-warn-orphans #-}
module Propellant.Lattices.Wide where

import Algebra.Lattice.Wide
import Control.Applicative

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
