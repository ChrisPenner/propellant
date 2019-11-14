{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module Propellant.Propagators where

import Propellant
import Control.Applicative

pBinOp :: (a -> b -> c) -> Cell f a -> Cell f b -> Cell f c -> Builder ()
pBinOp f inA@Cell{} inB@Cell{} out@Cell{} = do
    addNeighbour inA p
    addNeighbour inB p
  where
    p :: Propagator
    p = do
        result <- liftA2 (liftA2 f) (contents inA) (contents inB)
        addContent result out

equalizer :: Eq a => Cell f a -> Cell f a -> Cell f Bool -> Builder ()
equalizer a b c = pBinOp (==) a b c

adder :: (Num n) => Cell f n -> Cell f n -> Cell f n -> Builder ()
adder a b c = pBinOp (+) a b c

subtractor :: (Num n) => Cell f n -> Cell f n -> Cell f n -> Builder ()
subtractor = pBinOp (-)

multiplier :: (Num n) => Cell f n -> Cell f n -> Cell f n -> Builder ()
multiplier = pBinOp (*)

divider :: (Fractional n) => Cell f n -> Cell f n -> Cell f n -> Builder ()
divider = pBinOp (/)

constant :: Info f i => f i -> Cell f i -> Builder ()
constant i cell = scheduleB $ addContent i cell

constant' :: Info f i => i -> Cell f i -> Builder ()
constant' i cell = scheduleB $ addContent (pure i) cell

sum :: (Num n) => Cell f n -> Cell f n -> Cell f n -> Builder ()
sum inA inB total = do
    adder inA inB total
    subtractor total inA inB
    subtractor total inB inA

sub :: (Num n) => Cell f n -> Cell f n -> Cell f n -> Builder ()
sub inA inB total = do
    subtractor inA inB total
    adder total inB inA
    subtractor inA total inB

mult :: (Fractional n) => Cell f n -> Cell f n -> Cell f n -> Builder ()
mult inA inB total = do
    multiplier inA inB total
    divider total inB inA
    divider total inA inB

div :: (Fractional n) => Cell f n -> Cell f n -> Cell f n -> Builder ()
div inA inB total = do
    divider inA inB total
    multiplier inB total inA
    divider inA total inB

map :: (a -> b) -> Cell f a -> Cell f b -> Builder ()
map f inp@Cell{} out@Cell{} = do
    addNeighbour inp p
  where
    p :: Propagator
    p = do
       a <- contents inp
       addContent (fmap f a) out

(-!) :: (Num n) => Cell f n -> Cell f n -> Cell f n -> Builder ()
(-!) = sub

(+!) :: (Num n) => Cell f n -> Cell f n -> Cell f n -> Builder ()
(+!) = Propellant.Propagators.sum

(*!) :: (Fractional n) => Cell f n -> Cell f n -> Cell f n -> Builder ()
(*!) = mult

(/!) :: (Fractional n) => Cell f n -> Cell f n -> Cell f n -> Builder ()
(/!) = Propellant.Propagators.div

(=!) :: Cell f a -> (Cell f a -> Builder ()) -> Builder ()
(=!) a f = (f a)

store :: Info f a => (Cell f a -> Builder ()) -> Builder (Cell f a)
store f = do
    c <- emptyCell
    f c
    return c

-- (%~!) :: Info b => Cell f a -> (a -> b) -> Prop (Cell f b)
-- (%~!) a f = store (Propellant.Propagators.map f a)

(%~!) :: Cell f a -> (a -> b) -> Cell f b -> Builder ()
(%~!) a f out = Propellant.Propagators.map f a out

subscribe :: Cell f a -> (f a -> Prop ()) -> Builder ()
subscribe c f = do
    addNeighbour c p
  where
    p :: Propagator
    p = contents c >>= f

(!->) :: Cell f a -> (f a -> Prop ()) -> Builder ()
(!->) = subscribe
