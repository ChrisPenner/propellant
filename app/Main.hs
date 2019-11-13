module Main where

import Propellant
import Propellant.Propagators as P
import Algebra.Lattice.Wide
import Text.Printf

f2c :: (Fractional n, Eq n) => Cell (Wide n) -> Cell (Wide n) -> IO Propagator
f2c fahrenheit celsius = do
    thirtyTwo <- newCell Bottom
    fminus32 <- newCell Bottom
    five  <- newCell Bottom
    cTimesNine <- newCell Bottom
    nine  <- newCell Bottom
    return $ mconcat
      [ constant' 32 thirtyTwo
      , constant' 5 five
      , constant' 9 nine
      , sub fahrenheit thirtyTwo fminus32
      , mult fminus32 five cTimesNine
      , P.div cTimesNine nine celsius
      ]

main :: IO ()
main = do
    fahrenheit <- newCell Bottom
    celsius <- newCell Bottom
    p <- f2c fahrenheit celsius
    quiesce (p <> constant' (12 :: Double) celsius)
    Middle c <- readCell celsius
    Middle f <- readCell fahrenheit
    printf "%f Fahrenheit = %f Celsius\n" f c
    return ()
