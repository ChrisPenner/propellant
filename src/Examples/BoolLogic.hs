{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Examples.BoolLogic where

import Propellant
import Propellant.Propagators
import Propellant.Propagators.Logic
import Propellant.Lattices.Evidence
import Propellant.Lattices.Orphans ()
import Algebra.Lattice.Wide

main :: IO ()
main = do
    (cellA, cellB, aEqB) <- quiesce $ do
        cellA <- emptyCell @(Evidence String) @(Wide Int)
        cellB <- emptyCell
        aEqB <- emptyCell @(Evidence String) @(Wide Bool)
        aEqB =! (cellA =? cellB)
        -- constant' (3 :: Wide Int) cellA
        constant' 4 cellB
        constant' (pure True :: Wide Bool) aEqB
        return (cellA, cellB, aEqB)
    a :: (Evidence String (Wide Int)) <- readCell cellA
    b :: (Evidence String (Wide Int)) <- readCell cellB
    areEq <- readCell aEqB
    putStrLn $ "A"
    putStrLn $ showAllEvidence a
    putStrLn $ "B"
    putStrLn $ showAllEvidence b
    putStrLn $ "A = B"
    putStrLn $ showAllEvidence areEq

testSwitch :: IO ()
testSwitch = do
    out <- quiesce $ do
        input <- emptyCell @(Evidence String)
        control <- emptyCell
        output <- emptyCell
        switch control input output
        constant ("four" `implies` Middle (4 :: Int)) input
        -- constant' (10 :: Wide Int) input
        constant ("switcher" `implies` Middle True) control
        return (output)
    o <- readCell out
    putStrLn $ showAllEvidence o
