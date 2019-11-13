{-# LANGUAGE ScopedTypeVariables #-}
module Propellant.Lattices.Evidence where

import qualified Data.Set as S
import qualified Data.Map as M
import Algebra.Lattice

data Evidence e v = Evidence (M.Map e v)
                  | NoEvidence
                  | TotalContradiction
  deriving (Show, Eq)

implies :: e -> v -> Evidence e v
implies e v = Evidence (M.singleton e v)

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

showEvidence :: forall k v. (Ord k, Eq v, BoundedLattice v) => Evidence k v -> (S.Set k, v)
showEvidence TotalContradiction = (mempty, top)
showEvidence NoEvidence = (mempty, bottom)
showEvidence (Evidence m) = M.foldrWithKey go (mempty, bottom) m
  where
    go :: k -> v -> (S.Set k, v) -> (S.Set k, v)
    go k v e@(ks, vs)
      -- This element doesn't add any info
      | joinLeq v vs = e
      | joinLeq vs v = (S.singleton k, v)
      | otherwise = (S.insert k ks, v \/ vs)

-- test :: (S.Set String, Range Int)
-- test =  showEvidence . Evidence $ M.fromList
--   [
--     ("two", Range 1 10)
--   , ("one", Range 2 11)
--   , ("three", Range 3 8)
--   , ("four", Range 4 10)
--   ]
