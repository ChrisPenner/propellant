module Examples.Barometer where

import Propellant
import Propellant.Propagators
import Propellant.Lattices.Evidence
import Propellant.Lattices.Range
import Propellant.Lattices.Num
import Algebra.Lattice.Levitated
import Text.Printf
import qualified Data.Set as S

similarTriangles :: (Fractional n, Info n)
                 => (Cell n, Cell n) -- ^ First triangle (b, h)
                 -> (Cell n, Cell n) -- ^ Second triangle (b, h)
                 -> Builder ()
similarTriangles (x1, y1) (x2, y2) = do
    ratio1 <- store (x1 /! y1)
    ratio2 <- store (x2 /! y2)
    x1 =! (ratio2 *! y1)
    x2 =! (ratio1 *! y2)
    y1 =! (x1 /! ratio2)
    y2 =! (x2 /! ratio1)

main :: IO ()
main = do
    (buildingShadow, buildingHeight) <- quiesce $ do
        barometerHeight <- (emptyCell :: Builder (Cell (Evidence (S.Set String) (Range (Levitated Rational)))))
        barometerShadow <- emptyCell
        buildingHeight <- emptyCell
        buildingShadow <- emptyCell
        similarTriangles (barometerShadow, barometerHeight) (buildingShadow, buildingHeight)
        constant (S.singleton "shadow" `implies` Range 54 55) buildingShadow
        constant (S.singleton "shadow" `implies` Range 0.3 0.32) buildingHeight
        constant (S.singleton "shadow" `implies` Range 0.3 0.32) barometerShadow
        return (buildingShadow, buildingHeight)
    shadow <- readCell buildingShadow
    height <- readCell buildingHeight
    print (showEvidence shadow)
    print (showEvidence height)

-- main :: IO ()
-- main = do
--     (buildingShadow, buildingHeight) <- quiesce $ do
--         barometerHeight <- (emptyCell :: Builder (Cell (Evidence String (Range (Levitated Rational)))))
--         barometerShadow <- emptyCell
--         buildingHeight <- emptyCell
--         buildingShadow <- emptyCell
--         similarTriangles (barometerShadow, barometerHeight) (buildingShadow, buildingHeight)
--         constant ("shadow" `implies` Range 54 55) buildingShadow
--         constant ("shadow" `implies` Range 0.3 0.32) buildingHeight
--         constant ("shadow" `implies` Range 0.3 0.32) barometerShadow
--         return (buildingShadow, buildingHeight)
--     shadow <- readCell buildingShadow
--     height <- readCell buildingHeight
--     print (showEvidence shadow)
--     print (showEvidence height)
--     -- printf "%f Fahrenheit = %f Celsius\n" (fromRational shadow :: Float) (fromRational height :: Float)
