{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Propellant.Merge where

import Data.Ratio
import Numeric.Interval.Internal
import qualified Data.Set as S
import qualified Data.IntSet as IS

class Mergeable a where
  merge :: a -> a -> Merged a
  default merge :: (Eq a) => a -> a -> Merged a
  merge = eqMerge

data Merged a =
        Contradiction
      | NoChange a
      | Changed (S.Set IS.IntSet) a
  deriving (Show, Eq, Functor, Ord, Foldable, Traversable)

instance Applicative Merged where
  pure = NoChange
  Contradiction <*> _ = Contradiction
  _ <*> Contradiction = Contradiction
  NoChange f <*> NoChange a = NoChange $ f a
  Changed s f <*> NoChange a = Changed s $ f a
  NoChange f <*> Changed s a = Changed s $ f a
  Changed s f <*> Changed s' a = Changed (s <> s') $ f a

instance Monad Merged where
  return = pure
  Contradiction >>= _f = Contradiction
  Changed s a >>= f = case f a of
      Changed s' b -> Changed (s <> s') b
      NoChange b -> Changed s b
      Contradiction -> Contradiction
  NoChange a >>= f = f a



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
  merge (Just a) Nothing = Changed mempty (Just a)
  merge Nothing (Just a)= Changed mempty (Just a)
  merge (Just a) (Just b)= Just <$> merge a b

instance Ord n => Mergeable (Interval n) where
  old@(I l h) `merge` _new'@(I l' h')
    | h < l' || h' < l = Contradiction
    | h < l' || h' < l = Contradiction
  -- No information added
    | otherwise =
        let merged = I (max l l') (min h h')
         in if merged == old then NoChange merged
                         else Changed mempty merged
  merge Empty _ = NoChange Empty
  merge (I _ _) Empty = Changed mempty Empty
