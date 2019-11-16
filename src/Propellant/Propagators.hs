{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module Propellant.Propagators where

import Propellant.Cell
import Propellant.Prop
import Propellant.Merge
import Propellant.Scheduler
import Control.Applicative

liftP2 :: (a -> b -> c) -> Cell a -> Cell b -> Cell c -> Builder ()
liftP2 f inA inB out = do
    addNeighbour inA p
    addNeighbour inB p
  where
    p = do
      a <- contents inA
      b <- contents inB
      case liftA2 f a b of
          Nothing -> return ()
          Just result ->
              addContent result out

-- equalizer :: Eq a => Cell a -> Cell a -> Cell Bool -> Builder ()
-- equalizer a b c = pBinOp (==) a b c

adder :: (Num n) => Cell n -> Cell n -> Cell n -> Builder ()
adder = liftP2 (+)

subtractor :: (Num n) => Cell n -> Cell n -> Cell n -> Builder ()
subtractor = liftP2 (-)

multiplier :: (Num n) => Cell n -> Cell n -> Cell n -> Builder ()
multiplier = liftP2 (*)

divider :: (Fractional n) => Cell n -> Cell n -> Cell n -> Builder ()
divider = liftP2 (/)

constant :: a -> Cell a -> Builder ()
constant a cell = schedule $ addContent a cell

bisum :: (Num n) => Cell n -> Cell n -> Cell n -> Builder ()
bisum inA inB total = do
    adder inA inB total
    subtractor total inA inB
    subtractor total inB inA

bisub :: (Num n) => Cell n -> Cell n -> Cell n -> Builder ()
bisub inA inB total = do
    subtractor inA inB total
    adder total inB inA
    subtractor inA total inB

bimult :: (Fractional n) => Cell n -> Cell n -> Cell n -> Builder ()
bimult inA inB total = do
    multiplier inA inB total
    divider total inB inA
    divider total inA inB

bidiv :: (Fractional n) => Cell n -> Cell n -> Cell n -> Builder ()
bidiv inA inB total = do
    divider inA inB total
    multiplier inB total inA
    divider inA total inB

mapP :: (a -> b) -> Cell a -> Cell b -> Builder ()
mapP f inp out = do
    addNeighbour inp p
  where
    p :: Propagator
    p = do
       using inp $ \a -> addContent (f a) out

store :: Mergeable a => (Cell a -> Builder ()) -> Builder (Cell a)
store f = do
    c <- emptyCell
    f c
    return c


-- (-!) :: (Num n) => Cell n -> Cell n -> Cell n -> Builder ()
-- (-!) = sub

-- (+!) :: (Num n) => Cell n -> Cell n -> Cell n -> Builder ()
-- (+!) = Propellant.Propagators.sum

-- (*!) :: (Fractional n) => Cell n -> Cell n -> Cell n -> Builder ()
-- (*!) = mult

-- (/!) :: (Fractional n) => Cell n -> Cell n -> Cell n -> Builder ()
-- (/!) = Propellant.Propagators.div

(=!) :: Cell a -> (Cell a -> Builder ()) -> Builder ()
(=!) a f = (f a)

-- -- (%~!) :: Info b => Cell a -> (a -> b) -> Prop (Cell b)
-- -- (%~!) a f = store (Propellant.Propagators.map f a)

-- (%~!) :: Cell a -> (a -> b) -> Cell b -> Builder ()
-- (%~!) a f out = Propellant.Propagators.map f a out

-- subscribe :: Cell a -> (Evidence e a -> Prop ()) -> Builder ()
-- subscribe c f = do
--     addNeighbour c p
--   where
--     p :: Propagator
--     p = contents c >>= f

-- (!->) :: Cell a -> (Evidence e a -> Prop ()) -> Builder ()
-- (!->) = subscribe
