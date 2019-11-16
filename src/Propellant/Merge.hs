{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DefaultSignatures #-}
module Propellant.Merge where

import Data.Ratio
import Numeric.Interval.Internal

class Mergeable a where
  merge :: a -> a -> Merged a
  default merge :: (Eq a) => a -> a -> Merged a
  merge = eqMerge

data Merged a =
        Contradiction
      | NoChange a
      | Changed a
  deriving (Show, Eq, Functor)


eqMerge :: Eq a => a -> a -> Merged a
eqMerge a b | a == b = NoChange a
            | otherwise = Contradiction

instance Mergeable Int where
instance Mergeable Bool where

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

instance (Eq n) => Mergeable (Ratio n) where
  merge = eqMerge

instance Mergeable a => Mergeable (Maybe a) where
  merge Nothing Nothing = NoChange Nothing
  merge (Just a) Nothing = Changed (Just a)
  merge Nothing (Just a)= Changed (Just a)
  merge (Just a) (Just b)= Just <$> merge a b

instance Ord n => Mergeable (Interval n) where
  old@(I l h) `merge` _new'@(I l' h')
    | h < l' || h' < l = Contradiction
    | h < l' || h' < l = Contradiction
  -- No information added
    | otherwise =
        let merged = I (max l l') (min h h')
         in if merged == old then NoChange merged
                         else Changed merged
  merge Empty _ = NoChange Empty
  merge (I _ _) Empty = Changed Empty
