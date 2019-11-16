{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Propellant.Supports where

import Propellant.Merge
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Functor.Compose
import Control.Applicative
import Control.Monad

data Evidence e a = Evidence (M.Map (S.Set e) (Merged a))
  deriving (Show, Eq, Functor)

instance (Ord e, Mergeable a) => Semigroup (Evidence e a) where
  a <> b = superSet $ accumulateEvidence a b

-- Can make this smarter by ignoring combinations which aren't more precise than their
-- components
accumulateEvidence :: forall e v. (Ord e, Mergeable v) => Evidence e v -> Evidence e v -> Evidence e v
accumulateEvidence (Evidence a) (Evidence b) = Evidence combined -- clean combined
  where
    combined = M.unionWith go a b
    go :: Merged v -> Merged v -> Merged v
    go mx my = do
        x <- mx
        y <- my
        merge x y

superSet :: (Ord e, Mergeable v) => Evidence e v -> Evidence e v
superSet (Evidence m) = Evidence . M.fromList $ do
    (k, v) <- M.toList m
    (k', v') <- M.toList m
    case join (liftA2 merge v v') of
        Contradiction -> empty
        a -> return (k <> k', a)

instance (Ord e, Mergeable a) => Monoid (Evidence e a) where
  mempty = Evidence mempty

instance (Ord e) => Applicative (Evidence e) where
  pure v = Evidence (M.singleton mempty (pure v))
  (<*>) :: forall a b. Evidence e (a -> b) -> Evidence e a -> Evidence e b
  Evidence f <*> Evidence a = Evidence (M.fromList results)
    where
      results :: [(S.Set e, Merged b)]
      results = getCompose . getCompose $ (Compose . Compose) (M.toList f) <*> (Compose . Compose) (M.toList a)

instance (Ord e, Eq a, Mergeable a) => Mergeable (Evidence e a) where
  merge old new =
      let merged = old <> new
       in if merged == old then NoChange merged
                           else Changed merged

-- A Monad over evidence; not a proper monad because of Lattice requirement on contents
-- instance (Ord e) => Monad (Evidence e) where
(>>~) :: (Ord e, Mergeable b) => Evidence e a -> (a -> Evidence e b) -> Evidence e b
ev >>~ f = eJoin $ fmap f ev

eJoin :: (Ord e, Mergeable a) => Evidence e (Evidence e a) -> Evidence e a
eJoin (Evidence m) = foldMap flatten . M.toList $ m
  where
    flatten :: Ord e => (S.Set e, Merged (Evidence e b)) -> Evidence e b
    flatten (e, Contradiction) = Evidence $ M.singleton e Contradiction
    flatten (e, Changed (Evidence n)) = Evidence $ M.mapKeys (e <>) n
    flatten (e, NoChange (Evidence n)) = Evidence $ M.mapKeys (e <>) n

implies :: e -> v -> Evidence e v
implies e v = Evidence (M.singleton (S.singleton e) (pure v))


-- clean :: (Ord e, Eq b, Lattice b) => Evidence e b -> Evidence e b
-- clean (Evidence m) = Evidence . M.fromList $ do
--         (k, v) <- (M.toList m)
--         (k', v') <- (M.toList m)
--         if | k == k' -> return (k, v)
--            | joinLeq v v' -> return (k', v')
--            | joinLeq v' v -> return (k, v)
--            | otherwise -> return $ (k, v) \/ (k', v')

instance (Ord e, Monoid e, Num n) => Num (Evidence e n) where
  a + b = liftA2 (+) a b
  a - b = liftA2 (-) a b
  a * b = liftA2 (*) a b
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Ord e, Monoid e, Fractional n) => Fractional (Evidence e n) where
  fromRational = pure . fromRational
  recip = fmap recip

-- partialCompare :: (PartialOrd a) => a -> a -> Ordering
-- partialCompare a b
--   | not (comparable a  b) = EQ
--   | leq a b = LT
--   | otherwise = GT

-- showBestEvidence :: forall e v. (Show e, Show v, Eq v, Lattice v) => Evidence e v -> String
-- showBestEvidence (Evidence m) = uncurry showEvidenceLine . foldl1' findBest $ M.toList m
--   where
--     findBest :: (S.Set e, v) -> (S.Set e, v) -> (S.Set e, v)
--     findBest (e, v) (e', v')
--       | joinLeq v v' = (e', v')
--       | joinLeq v' v = (e, v)
--       | otherwise = (e, v)

-- showAllEvidence :: forall e v. (Show e, Show v) => Evidence e v -> String
-- showAllEvidence (Evidence m) = M.foldMapWithKey showEvidenceLine m

-- showEvidenceLine :: (Show e, Show v) => S.Set e -> v -> String
-- showEvidenceLine es v = (intercalate ", " . fmap show $ (S.toList es)) <> ":\n -> " <> show v <> "\n"

-- evidenceWithout :: (Ord e) => e -> Evidence e v -> Evidence e v
-- evidenceWithout e (Evidence m) = Evidence (flip M.filterWithKey m $ \es _ -> not $ e `S.member` es)

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
