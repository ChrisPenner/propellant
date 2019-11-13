{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
module Propellant.Lattices.Evidence where

import qualified Data.Set as S
import qualified Data.Map as M
import Algebra.Lattice
import Data.Functor.Compose

data Evidence e v = Evidence (M.Map (S.Set e) v)
                  | NoEvidence
                  | TotalContradiction
  deriving (Show, Eq, Functor)

instance (Ord e) => Applicative (Evidence e) where
  pure v = Evidence (M.singleton mempty v)
  (<*>) :: forall a b. Evidence e (a -> b) -> Evidence e a -> Evidence e b
  TotalContradiction <*> _ = TotalContradiction
  _ <*> TotalContradiction = TotalContradiction
  _ <*> NoEvidence = NoEvidence
  NoEvidence <*> _ = NoEvidence
  Evidence f <*> Evidence a = Evidence (M.fromList results)
    where
      results :: [(S.Set e, b)]
      results = getCompose $ Compose (M.toList f) <*> Compose (M.toList a)

implies :: e -> v -> Evidence e v
implies e v = Evidence (M.singleton (S.singleton e) v)

instance (Lattice v, Ord e) => Lattice (Evidence e v) where
  (/\) = error "Cheating lattice"
  TotalContradiction \/ _ = TotalContradiction
  _ \/ TotalContradiction = TotalContradiction
  NoEvidence \/ e = e
  e \/ NoEvidence = e
  Evidence m \/ Evidence n = Evidence (M.unionWith (\/) m n)

instance (Lattice v, Ord e) => BoundedJoinSemiLattice (Evidence e v) where
  bottom = NoEvidence

instance (Ord e, Lattice v) => BoundedMeetSemiLattice (Evidence e v) where
  top = TotalContradiction

showEvidence :: forall e v. (Ord e, Eq v, BoundedLattice v) => Evidence e v -> (S.Set e, v)
showEvidence TotalContradiction = (mempty, top)
showEvidence NoEvidence = (mempty, bottom)
showEvidence (Evidence m) = M.foldrWithKey go (mempty, bottom) m
  where
    go :: S.Set e -> v -> (S.Set e, v) -> (S.Set e, v)
    go k v e@(ks, vs)
      -- This element doesn't add any info
      | joinLeq v vs = e
      | joinLeq vs v = (k, v)
      | otherwise = (S.union k ks, v \/ vs)

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
