{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}

module Propellant.Lattices.Evidence where

import qualified Data.Set as S
import qualified Data.Map as M
import Algebra.Lattice
import Propellant.Lattices.Range
import Data.Functor.Compose
import Data.List
import Control.Applicative

data Evidence e v = Evidence (M.Map (S.Set e) v)
  deriving (Show, Eq, Functor)

instance (Lattice v, Ord e, Eq v) => Semigroup (Evidence e v) where
  (<>) = (\/)

instance (Lattice v, Ord e, Eq v) => Monoid (Evidence e v) where
  mempty = bottom


instance (Ord e) => Applicative (Evidence e) where
  pure v = Evidence (M.singleton mempty v)
  (<*>) :: forall a b. Evidence e (a -> b) -> Evidence e a -> Evidence e b
  Evidence f <*> Evidence a = Evidence (M.fromList results)
    where
      results :: [(S.Set e, b)]
      results = getCompose $ Compose (M.toList f) <*> Compose (M.toList a)

-- A Monad over evidence; not a proper monad because of Lattice requirement on contents
(>>~) :: forall a b e. (Ord e, Lattice b, Eq b) => Evidence e a -> (a -> Evidence e b) -> Evidence e b
ev >>~ f = eJoin $ fmap f ev

eJoin :: (Lattice a, Ord e, Eq a) => Evidence e (Evidence e a) -> Evidence e a
eJoin (Evidence m) = foldMap flatten . M.toList $ m
  where
    flatten :: Ord e => (S.Set e, Evidence e b) -> Evidence e b
    flatten (e, Evidence n) = Evidence $ M.mapKeys (e <>) n


-- ([("a", 1), ("b", 2)])

implies :: e -> v -> Evidence e v
implies e v = Evidence (M.singleton (S.singleton e) v)

instance (Lattice v, Ord e, Eq v) => Lattice (Evidence e v) where
  (/\) = error "Evidence has no Meet"
  m \/ n = powerSet m n

-- Can make this smarter by ignoring combinations which aren't more precise than their
-- components
powerSet :: (Ord e, Eq v, Lattice v) => Evidence e v -> Evidence e v -> Evidence e v
powerSet (Evidence a) (Evidence b) = clean combined
  where
    combined = Evidence (M.unionWith (\/) a b)

clean :: (Ord e, Eq b, Lattice b) => Evidence e b -> Evidence e b
clean (Evidence m) = Evidence . M.fromList $ do
        (k, v) <- (M.toList m)
        (k', v') <- (M.toList m)
        if | k == k' -> return (k, v)
           | joinLeq v v' -> return (k', v')
           | joinLeq v' v -> return (k, v)
           | otherwise -> return $ (k, v) \/ (k', v')

instance (Lattice v, Ord e, Eq v) => BoundedJoinSemiLattice (Evidence e v) where
  bottom = Evidence mempty

instance (Ord e, Eq n, Lattice n, Monoid e, Num n) => Num (Evidence e n) where
  a + b = clean $ liftA2 (+) a b
  a - b = clean $ liftA2 (-) a b
  a * b = clean $ liftA2 (*) a b
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Ord e, Monoid e, Fractional n, Eq n, Lattice n) => Fractional (Evidence e n) where
  fromRational = pure . fromRational
  recip = fmap recip

-- partialCompare :: (PartialOrd a) => a -> a -> Ordering
-- partialCompare a b
--   | not (comparable a  b) = EQ
--   | leq a b = LT
--   | otherwise = GT

showBestEvidence :: forall e v. (Show e, Show v, Eq v, Lattice v) => Evidence e v -> String
showBestEvidence (Evidence m) = uncurry showEvidenceLine . foldl1' findBest $ M.toList m
  where
    findBest :: (S.Set e, v) -> (S.Set e, v) -> (S.Set e, v)
    findBest (e, v) (e', v')
      | joinLeq v v' = (e', v')
      | joinLeq v' v = (e, v)
      | otherwise = (e, v)

showAllEvidence :: forall e v. (Show e, Show v) => Evidence e v -> String
showAllEvidence (Evidence m) = M.foldMapWithKey showEvidenceLine m

showEvidenceLine :: (Show e, Show v) => S.Set e -> v -> String
showEvidenceLine es v = (intercalate ", " . fmap show $ (S.toList es)) <> ":\n -> " <> show v <> "\n"

evidenceWithout :: (Ord e) => e -> Evidence e v -> Evidence e v
evidenceWithout e (Evidence m) = Evidence (flip M.filterWithKey m $ \es _ -> not $ e `S.member` es)

-- one, two, three :: Evidence String (Range Int)
-- one = "one" `implies` (Range 1 10)
-- two = "two" `implies` (Range 3 12)
-- three = "three" `implies` (Range 4 6)
-- test :: String
-- test =  showAllEvidence . joins $
--   [ one
--   , two
--   , three
--   , one + two
--   ]
-- -- expected:
-- -- >>> test
-- -- (fromList ["four","three"],Range {rangeMin = 4, rangeMax = 8})
