module Main where

import Propellant
import Propellant.Propagators as P
import Propellant.Lattices.Range as P
import Algebra.Lattice.Wide as W
import Algebra.Lattice.Lifted as L
import Text.Printf

f2c :: (Fractional n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> Propagator
f2c fahrenheit celsius = do
    thirtyTwo <- newCell W.Bottom
    fminus32 <- newCell W.Bottom
    five  <- newCell W.Bottom
    cTimesNine <- newCell W.Bottom
    nine  <- newCell W.Bottom
    constant' 32 thirtyTwo
    constant' 5 five
    constant' 9 nine
    sub fahrenheit thirtyTwo fminus32
    mult fminus32 five cTimesNine
    P.div cTimesNine nine celsius

main :: IO ()
main = do
    (celsius, fahrenheit) <- quiesce $ do
        fahrenheit <- newCell W.Bottom
        celsius <- newCell W.Bottom
        f2c fahrenheit celsius
        constant' (12 :: Double) celsius
        return (celsius, fahrenheit)
    Middle c <- readCell celsius
    Middle f <- readCell fahrenheit
    printf "%f Fahrenheit = %f Celsius\n" f c

-- main :: IO ()
-- main = do
--     a <- quiesce $ do
--         a <- newCell L.Bottom
--         -- b <- newCell L.Bottom
--         constant (Lift (Range 12 20 :: Range Double)) a
--         constant (Lift (Range 15 30 :: Range Double)) a
--         constant (Lift (Range 15 21 :: Range Double)) a
--         liftSTM $ readCellT a
--     print a
--     -- Lift (Range mna mxa) <- readCell a
--     -- printf "min: %f max: %f\n" mna mxa
