{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
module Propellant.Lattices.Evidence where

import qualified Data.Set as S
import qualified Data.Map as M
import Algebra.Lattice
import Propellant.Lattices.Range
import Data.Functor.Compose
import Data.List
import Algebra.PartialOrd
import Control.Applicative

data Evidence e v = Evidence (M.Map (S.Set e) v)
  deriving (Show, Eq, Functor)

instance (Ord e) => Applicative (Evidence e) where
  pure v = Evidence (M.singleton mempty v)
  (<*>) :: forall a b. Evidence e (a -> b) -> Evidence e a -> Evidence e b
  Evidence f <*> Evidence a = Evidence (M.fromList results)
    where
      results :: [(S.Set e, b)]
      results = getCompose $ Compose (M.toList f) <*> Compose (M.toList a)

implies :: e -> v -> Evidence e v
implies e v = Evidence (M.singleton (S.singleton e) v)

instance (Lattice v, Ord e) => Lattice (Evidence e v) where
  (/\) = error "Evidence has no Meet"
  m \/ n = powerSet m n

powerSet :: (Ord e, Lattice v) => Evidence e v -> Evidence e v -> Evidence e v
powerSet (Evidence a) (Evidence b) = liftA2 (\/) combined combined
  where
    combined = Evidence $ M.unionWith (\/) a b

instance (Lattice v, Ord e) => BoundedJoinSemiLattice (Evidence e v) where
  bottom = Evidence mempty

partialCompare :: (PartialOrd a) => a -> a -> Ordering
partialCompare a b
  | not (comparable a  b) = EQ
  | leq a b = LT
  | otherwise = GT

showBestEvidence :: (Show e, Show v, Ord e, PartialOrd v) => Evidence e v -> String
showBestEvidence (Evidence m) = uncurry showEvidenceLine . maximumBy partialCompare $ M.toList m

showAllEvidence :: forall e v. (Show e, Show v) => Evidence e v -> String
showAllEvidence (Evidence m) = M.foldMapWithKey showEvidenceLine m

showEvidenceLine :: (Show e, Show v) => S.Set e -> v -> String
showEvidenceLine es v = (intercalate ", " . fmap show $ (S.toList es)) <> ":\n -> " <> show v <> "\n"


one = "one" `implies` (Range 1 10) :: Evidence String (Range Int)
two = "two" `implies` (Range 3 12) :: Evidence String (Range Int)

-- test :: (S.Set String, Range Int)
-- test =  showEvidence  . joins $
--   [
--     ("two" `implies` Range 1 10)
--   , ("one" `implies` Range 2 11)
--   , ("three" `implies` Range 3 8)
--   , ("four" `implies` Range 4 10)
--   ]
-- -- expected:
-- -- >>> test
-- -- (fromList ["four","three"],Range {rangeMin = 4, rangeMax = 8})
