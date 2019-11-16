{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Propellant.Propagators.Logic where

import Propellant
import Control.Monad

(=?) :: Eq a => Cell a -> Cell a -> Cell Bool -> Builder ()
(=?) inA inB out = do
    let p :: Propagator = do
        using inA $ \a -> using inB $ \b -> do
            addContent (a == b) out
        usingWhen out id $ \_areEqual -> do
            using inA $ \a -> addContent a inB
            using inB $ \b -> addContent b inA
    addNeighbour inA p
    addNeighbour inB p

require :: Cell Bool -> Builder ()
require = constant True

forbid :: Cell Bool -> Builder ()
forbid = constant False

-- distinct :: (Ord e, Eq a, Foldable t) => t (Cell e (Wide a)) -> Builder ()
-- distinct (toList -> cells) = for_ pairs $ \(a, b) -> do
--     x <- emptyCell
--     x =! (a =? b)
--     forbid x
--   where
--     pairs = do
--         a <- cells
--         b <- cells
--         guard (a /= b)
--         return (a, b)

switch :: Cell Bool
       -> Cell a
       -> Cell a
       -> Builder ()
switch predicateCell inputCell@Cell{} outputCell@Cell{} = do
    let p :: Propagator = do
        using predicateCell $ \shouldPropagate -> do
            when shouldPropagate $ do
                using inputCell $ \input -> do
                    addContent input outputCell
    addNeighbour predicateCell p
    addNeighbour inputCell p

notCell :: Cell Bool -> Cell Bool -> Builder ()
notCell inp out = do
    out =! mapP not inp

conditional :: Cell Bool
            -> Cell a
            -> Cell a
            -> Cell a
            -> Builder ()
conditional control onTrue onFalse output = do
    notControl <- store (notCell control)
    output =! switch control onTrue
    output =! switch notControl onFalse

-- oneOf :: (Foldable t, Mergeable a) => t a -> Cell a -> Builder ()
-- oneOf (toList -> values) out = do
--     cells <- for values newCell
--     oneOfTheCells cells out


-- oneOfTheCells :: Foldable t => t (Cell e a) -> Cell e a -> Builder ()
-- oneOfTheCells = undefined
