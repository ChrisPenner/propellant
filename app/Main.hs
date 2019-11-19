module Main where

main :: IO ()
main = return ()

-- import Propellant
-- import Propellant.Propagators as P
-- import Propellant.Lattices.Range as P
-- import Propellant.Lattices.Wide
-- import Algebra.Lattice.Wide as W
-- import Algebra.Lattice.Lifted as L
-- import Text.Printf

-- f2c :: Cell (Wide Rational) -> Cell (Wide Rational) -> Builder ()
-- f2c fahrenheit celsius = do
--     thirtyTwo <- newCell (pure 32)
--     fminus32 <- emptyCell
--     five  <- newCell (pure 5)
--     cTimesNine <- emptyCell
--     nine  <- newCell (pure 9)
--     fminus32 =! (fahrenheit -! thirtyTwo)
--     cTimesNine =! (fminus32 *! five)
--     celsius =! (cTimesNine /! nine)

-- f2c' :: Cell (Wide Rational) -> Cell (Wide Rational) -> Builder ()
-- f2c' fahrenheit celsius = do
--     thirtyTwo <- store $ constant' 32
--     five  <- store $ constant' 5
--     nine  <- store $ constant' 9
--     fminus32 <- store $ fahrenheit -! thirtyTwo
--     cTimesNine <- store $ fminus32 *! five
--     celsius =! (cTimesNine /! nine)

-- f2c'' :: Cell (Wide Rational) -> Cell (Wide Rational) -> Builder ()
-- f2c'' fahrenheit celsius = do
--     fahrenheit !-> \f ->
--         addContent ((f - 32) * (5 / 9)) celsius
--     celsius !-> \c ->
--         addContent ((c * (9 / 5)) + 32) fahrenheit

-- main :: IO ()
-- main = do
--     (celsius, fahrenheit) <- quiesce $ do
--         fahrenheit <- newCell W.Bottom
--         celsius <- newCell W.Bottom
--         f2c'' fahrenheit celsius
--         constant' (12 :: Rational) celsius
--         return (celsius, fahrenheit)
--     Middle c <- readCell celsius
--     Middle f <- readCell fahrenheit
--     printf "%f Fahrenheit = %f Celsius\n" (fromRational f :: Float) (fromRational c :: Float)

-- -- main :: IO ()
-- -- main = do
-- --     a <- quiesce $ do
-- --         a <- newCell L.Bottom
-- --         -- b <- newCell L.Bottom
-- --         constant (Lift (Range 12 20 :: Range Double)) a
-- --         constant (Lift (Range 15 30 :: Range Double)) a
-- --         constant (Lift (Range 15 21 :: Range Double)) a
-- --         liftSTM $ readCellT a
-- --     print a
-- --     -- Lift (Range mna mxa) <- readCell a
-- --     -- printf "min: %f max: %f\n" mna mxa
