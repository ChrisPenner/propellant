{-# LANGUAGE ScopedTypeVariables #-}
module Propellant.Propagators where

import Propellant
import Algebra.Lattice.Wide
import Control.Applicative

pBinOp :: Info i => (i -> i -> i) -> Cell i -> Cell i -> Cell i -> Propagator
pBinOp f inA inB out = do
    addNeighbour inA p
    addNeighbour inB p
  where
    p :: Propagator
    p = do
        result <- liftA2 f (contents inA) (contents inB)
        addContent result out

adder :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Propagator
adder a b c = pBinOp (liftA2 (+)) a b c

subtractor :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Propagator
subtractor = pBinOp (liftA2 (-))

multiplier :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Propagator
multiplier = pBinOp (liftA2 (*))

divider :: (Fractional n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Propagator
divider = pBinOp (liftA2 (/))

constant :: Info i => i -> Cell i -> Propagator
constant i cell = addContent i cell

constant' :: (Eq i) => i -> Cell (Wide i) -> Propagator
constant' i cell = addContent (Middle i) cell

sum :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Propagator
sum inA inB total =
    adder inA inB total
    <> subtractor total inA inB
    <> subtractor total inB inA

sub :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Propagator
sub inA inB total =
    subtractor inA inB total
    <> adder total inB inA
    <> subtractor inA total inB

mult :: (Fractional n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Propagator
mult inA inB total =
    multiplier inA inB total
    <> divider total inB inA
    <> divider total inA inB

div :: (Fractional n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Propagator
div inA inB total =
    divider inA inB total
    <> multiplier inB total inA
    <> divider inA total inB

eq :: (Info a) => Cell a -> Cell a -> Propagator
eq inA inB = do
    addNeighbour inA p
    addNeighbour inB p
  where
    p :: Propagator
    p = do
        a <- contents inA
        b <- contents inB
        addContent a inB
        addContent b inA

map :: (Info b) => (a -> b) -> Cell a -> Cell b -> Propagator
map f inp out = do
    addNeighbour inp p
  where
    p :: Propagator
    p = do
       a <- contents inp
       addContent (f a) out

(-!) :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Propagator
(-!) = sub

(+!) :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Propagator
(+!) = Propellant.Propagators.sum

(*!) :: (Fractional n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Propagator
(*!) = mult

(/!) :: (Fractional n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Propagator
(/!) = Propellant.Propagators.div

(=!) :: Cell a -> (Cell a -> Propagator) -> Propagator
(=!) a f = (f a)

(==!) :: (Info a) => Cell a -> Cell a -> Propagator
(==!) = eq

store :: Info a => (Cell a -> Propagator) -> Prop (Cell a)
store f = do
    c <- emptyCell
    f c
    return c

-- (%~!) :: Info b => Cell a -> (a -> b) -> Prop (Cell b)
-- (%~!) a f = store (Propellant.Propagators.map f a)

(%~!) :: Info b => Cell a -> (a -> b) -> Cell b -> Propagator
(%~!) a f out = Propellant.Propagators.map f a out
