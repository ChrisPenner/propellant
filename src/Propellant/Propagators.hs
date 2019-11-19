{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module Propellant.Propagators where

import Propellant.Cell
import Propellant.Prop
import Propellant.Merge
import Propellant.Scheduler
import Control.Applicative

liftP2 :: (Ord c) => (a -> b -> c) -> Cell e a -> Cell e b -> Cell e c -> Builder ()
liftP2 f inA inB out = do
    addNeighbour inA p
    addNeighbour inB p
  where
    p = do
      a <- contents inA
      b <- contents inB
      addContent (liftA2 f a b) out

equalizer :: Eq a => Cell e a -> Cell e a -> Cell e Bool -> Builder ()
equalizer a b c = liftP2 (==) a b c

adder :: (Num n) => Cell e n -> Cell e n -> Cell e n -> Builder ()
adder = liftP2 (+)

subtractor :: (Num n) => Cell e n -> Cell e n -> Cell e n -> Builder ()
subtractor = liftP2 (-)

multiplier :: (Num n) => Cell e n -> Cell e n -> Cell e n -> Builder ()
multiplier = liftP2 (*)

divider :: (Fractional n) => Cell e n -> Cell e n -> Cell e n -> Builder ()
divider = liftP2 (/)

constant :: a -> Cell e a -> Builder ()
constant a cell = schedule $ addContent a cell

bisum :: (Num n) => Cell e n -> Cell e n -> Cell e n -> Builder ()
bisum inA inB total = do
    adder inA inB total
    subtractor total inA inB
    subtractor total inB inA

bisub :: (Num n) => Cell e n -> Cell e n -> Cell e n -> Builder ()
bisub inA inB total = do
    subtractor inA inB total
    adder total inB inA
    subtractor inA total inB

bimult :: (Fractional n) => Cell e n -> Cell e n -> Cell e n -> Builder ()
bimult inA inB total = do
    multiplier inA inB total
    divider total inB inA
    divider total inA inB

bidiv :: (Fractional n) => Cell e n -> Cell e n -> Cell e n -> Builder ()
bidiv inA inB total = do
    divider inA inB total
    multiplier inB total inA
    divider inA total inB

mapP :: (a -> b) -> Cell e a -> Cell e b -> Builder ()
mapP f inp out = do
    addNeighbour inp p
  where
    p :: Propagator
    p = do
       using inp $ \a -> addContent (f a) out

store :: Mergeable a => (Cell e a -> Builder ()) -> Builder (Cell e a)
store f = do
    c <- emptyCell
    f c
    return c


-- (-!) :: (Num n) => Cell e n -> Cell e n -> Cell e n -> Builder ()
-- (-!) = sub

-- (+!) :: (Num n) => Cell e n -> Cell e n -> Cell e n -> Builder ()
-- (+!) = Propellant.Propagators.sum

-- (*!) :: (Fractional n) => Cell e n -> Cell e n -> Cell e n -> Builder ()
-- (*!) = mult

-- (/!) :: (Fractional n) => Cell e n -> Cell e n -> Cell e n -> Builder ()
-- (/!) = Propellant.Propagators.div

(=!) :: Cell e a -> (Cell e a -> Builder ()) -> Builder ()
(=!) a f = (f a)

-- -- (%~!) :: Info b => Cell e a -> (a -> b) -> Prop (Cell e b)
-- -- (%~!) a f = store (Propellant.Propagators.map f a)

-- (%~!) :: Cell e a -> (a -> b) -> Cell e b -> Builder ()
-- (%~!) a f out = Propellant.Propagators.map f a out

-- subscribe :: Cell e a -> (Evidence e a -> Prop ()) -> Builder ()
-- subscribe c f = do
--     addNeighbour c p
--   where
--     p :: Propagator
--     p = contents c >>= f

-- (!->) :: Cell e a -> (Evidence e a -> Prop ()) -> Builder ()
-- (!->) = subscribe
