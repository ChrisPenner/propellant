module Propellant.Merge where

import Data.Ratio

class Mergeable a where
  merge :: a -> a -> Merged a

data Merged a =
        Contradiction
      | NoChange a
      | Changed a
  deriving (Show, Eq)


eqMerge :: Eq a => a -> a -> Merged a
eqMerge a b | a == b = NoChange a
            | otherwise = Contradiction

instance Mergeable Int where
  merge = eqMerge

instance Mergeable Float where
    merge a b
        | isNaN a && isNaN b                     = NoChange a
        | isInfinite a && isInfinite b && a == b = NoChange a
        | abs (a-b) < 1e-6                       = NoChange a
        | otherwise = Contradiction

instance Mergeable Double where
    merge a b
        | isNaN a && isNaN b                     = NoChange a
        | isInfinite a && isInfinite b && a == b = NoChange a
        | abs (a-b) < 1e-9                       = NoChange a
        | otherwise = Contradiction

instance (Eq n ) => Mergeable (Ratio n) where
  merge = eqMerge
