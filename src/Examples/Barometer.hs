{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Examples.Barometer where

import Propellant
import Propellant.Propagators
import Propellant.Lattices.Evidence
import Propellant.Lattices.Range
import Propellant.Lattices.Orphans
import Text.Printf
import qualified Data.Set as S

-- fallDuration :: Fractional n => Cell n -> Cell n -> Builder ()
-- fallDuration time height = do
--     gravity <- (newCell (Range 9.789 9.832))
--     oneHalf <- newCell 0.5
--   (let ((g (make-cell))
--         (one-half (make-cell))
--         (t^2 (make-cell))
--         (gt^2 (make-cell)))
-- ((constant (make-interval 9.789 9.832)) g) ((constant (make-interval 1/2 1/2)) one-half) (quadratic t t^2)
-- (product g t^2 gt^2)
--     (product one-half gt^2 h)))

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
    (buildingHeight :: Cell (Evidence String (Range Rational)))
        <- quiesce $ do
        barometerHeight <- emptyCell
        barometerShadow <- emptyCell
        buildingHeight <- emptyCell
        buildingShadow <- emptyCell
        similarTriangles (barometerShadow, barometerHeight) (buildingShadow, buildingHeight)
        constant ("shadow" `implies` Range 54.9 55.1) buildingShadow
        constant ("blueprints" `implies` Range 54.8 55) buildingShadow
        constant ("shadow" `implies` Range 0.3 0.32) barometerHeight
        -- constant ("measurement" `implies` Range 0.31 0.32) barometerHeight
        constant ("shadow" `implies` Range 0.36 0.37) barometerShadow
        return buildingHeight
    height <- readCell buildingHeight
    let floatHeight = (fmap . fmap) (fromRational @Double) height
    -- print (fmap (fromRational @Double) <$> showEvidence height)
    putStrLn $ showAllEvidence floatHeight
    putStrLn $ showBestEvidence floatHeight
    -- print $ height
