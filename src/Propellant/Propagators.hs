{-# LANGUAGE ScopedTypeVariables #-}
module Propellant.Propagators where

import Propellant
import Algebra.Lattice.Wide
import Control.Applicative

pBinOp :: Info i => (i -> i -> i) -> Cell i -> Cell i -> Cell i -> Builder ()
pBinOp f inA inB out = do
    addNeighbour inA p
    addNeighbour inB p
  where
    p :: Propagator
    p = do
        result <- liftA2 f (contents inA) (contents inB)
        addContent result out

adder :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Builder ()
adder a b c = pBinOp (liftA2 (+)) a b c

subtractor :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Builder ()
subtractor = pBinOp (liftA2 (-))

multiplier :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Builder ()
multiplier = pBinOp (liftA2 (*))

divider :: (Fractional n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Builder ()
divider = pBinOp (liftA2 (/))

constant :: Info i => i -> Cell i -> Builder ()
constant i cell = scheduleB $ addContent i cell

constant' :: (Eq i) => i -> Cell (Wide i) -> Builder ()
constant' i cell = scheduleB $ addContent (Middle i) cell

sum :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Builder ()
sum inA inB total = do
    adder inA inB total
    subtractor total inA inB
    subtractor total inB inA

sub :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Builder ()
sub inA inB total = do
    subtractor inA inB total
    adder total inB inA
    subtractor inA total inB

mult :: (Fractional n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Builder ()
mult inA inB total = do
    multiplier inA inB total
    divider total inB inA
    divider total inA inB

div :: (Fractional n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Builder ()
div inA inB total = do
    divider inA inB total
    multiplier inB total inA
    divider inA total inB

eq :: (Info a) => Cell a -> Cell a -> Builder ()
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

map :: (Info b) => (a -> b) -> Cell a -> Cell b -> Builder ()
map f inp out = do
    addNeighbour inp p
  where
    p :: Propagator
    p = do
       a <- contents inp
       addContent (f a) out

(-!) :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Builder ()
(-!) = sub

(+!) :: (Num n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Builder ()
(+!) = Propellant.Propagators.sum

(*!) :: (Fractional n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Builder ()
(*!) = mult

(/!) :: (Fractional n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Cell (Wide n) -> Builder ()
(/!) = Propellant.Propagators.div

(=!) :: Cell a -> (Cell a -> Builder ()) -> Builder ()
(=!) a f = (f a)

(==!) :: (Info a) => Cell a -> Cell a -> Builder ()
(==!) = eq

store :: Info a => (Cell a -> Builder ()) -> Builder (Cell a)
store f = do
    c <- emptyCell
    f c
    return c

-- (%~!) :: Info b => Cell a -> (a -> b) -> Prop (Cell b)
-- (%~!) a f = store (Propellant.Propagators.map f a)

(%~!) :: Info b => Cell a -> (a -> b) -> Cell b -> Builder ()
(%~!) a f out = Propellant.Propagators.map f a out
