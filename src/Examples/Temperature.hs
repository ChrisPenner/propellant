module Examples.Temperature where

import Propellant
import Text.Printf

f2c :: Cell Rational -> Cell Rational -> Builder ()
f2c fahrenheit celsius = do
    thirtyTwo <- newCell 32
    fminus32 <- emptyCell
    five  <- newCell 5
    cTimesNine <- emptyCell
    nine  <- newCell 9
    fminus32 =! (fahrenheit `bisub` thirtyTwo)
    cTimesNine =! (fminus32 `bimult` five)
    celsius =! (cTimesNine `bidiv` nine)

f2c' :: Cell Rational -> Cell Rational -> Builder ()
f2c' fahrenheit celsius = do
    thirtyTwo <- store $ constant 32
    five  <- store $ constant 5
    nine  <- store $ constant 9
    fminus32 <- store $ fahrenheit `bisub` thirtyTwo
    cTimesNine <- store $ fminus32 `bimult` five
    celsius =! (cTimesNine `bidiv` nine)

main :: IO ()
main = do
    (celsius, fahrenheit) <- quiesce $ do
        fahrenheit <- emptyCell
        celsius <- emptyCell
        f2c' fahrenheit celsius
        -- constant (100 :: Rational) celsius
        constant (33 :: Rational) fahrenheit
        return (celsius, fahrenheit)
    Just c <- readCell celsius
    Just f <- readCell fahrenheit
    printf "%f Fahrenheit = %f Celsius\n" (fromRational f :: Float) (fromRational c :: Float)
