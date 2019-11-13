module Main where

import Propellant
import Propellant.Propagators as P
import Algebra.Lattice.Wide
import Control.Concurrent
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
      , subtractor fahrenheit thirtyTwo fminus32
      , multiplier fminus32 five cTimesNine
      , divider cTimesNine nine celsius
      ]


-- main :: IO ()
-- main = do
--     fahrenheit <- newCell Bottom
--     celsius <- newCell Bottom
--     p <- f2c fahrenheit celsius
--     quiesce (p <> constant' (12 :: Double) celsius)
--     readCell fahrenheit >>= print
--     return ()

main :: IO ()
main = do
    a <- newCell Bottom
    b <- newCell Bottom
    out <- newCell Bottom
    quiesce (constant' (3 :: Double) out <> constant' (10 :: Double) b <> P.mult a b out)
    -- quiesce (constant' (1 :: Int) a)
    Middle a' <- readCell a
    Middle b' <- readCell b
    Middle out' <- readCell out
    printf "%f * %f = %f\n" a' b' out'
