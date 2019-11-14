{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Propellant.Lattices.Hypothetical where

import qualified Data.Set as S
import Algebra.Lattice
import Propellant
import Control.Applicative

-- data Guess a = Hypothetical a | Definitely a
--   deriving (Show, Eq, Ord, Functor)

-- instance Applicative Guess where
--   pure a = Definitely a
--   Hypothetical f <*> Hypothetical a = Hypothetical (f a)
--   Definitely f <*> Hypothetical a = Hypothetical (f a)
--   Hypothetical f <*> Definitely a = Hypothetical (f a)
--   Definitely f <*> Definitely a = Definitely (f a)

data Quasi a =
    Quasi { options :: S.Set a
          , banned  :: S.Set a
          } deriving Eq

instance Ord a => Lattice (Quasi a) where
  _ \/ _ = error "define meet for Quasi"
  Quasi optA banA /\ Quasi optB banB = Quasi (optA <> optB) (banA <> banB)

-- binaryAmb :: Cell a -> Builder ()
-- binaryAmb c = do
--     truePremise <- emptyCell
--     falsePremise <- emptyCell
--     let ambChoice = do
--         undefined
--     addNeighbour c ambChoice

-- require :: Cell Bool -> Builder ()
-- require c = do


-- (=?) :: (forall x. BoundedJoinSemiLattice (f x), Eq a, Applicative f, Eq (f Bool)) => Cell (f a) -> Cell (f a) -> Cell (f Bool) -> Propagator
-- (=?) inA inB out = do
--     a <- contents inA
--     b <- contents inB
--     result <- contents out
--     addContent (liftA2 (==) a b) out
--     addContent (liftA2 (==) a b) out
