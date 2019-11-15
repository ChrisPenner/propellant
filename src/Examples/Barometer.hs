{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Examples.Barometer where

import Propellant
import Text.Printf
import qualified Data.Set as S

-- fallDuration :: Fractional n => Cell -> Cell -> Builder ()
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

similarTriangles :: (Fractional n, Mergeable n)
                 => (Cell n, Cell n) -- ^ First triangle (b, h)
                 -> (Cell n, Cell n) -- ^ Second triangle (b, h)
                 -> Builder ()
similarTriangles (x1, y1) (x2, y2) = do
    ratio1 <- store (x1 `bidiv` y1)
    ratio2 <- store (x2 `bidiv` y2)
    x1 =! (ratio2 `bimult` y1)
    x2 =! (ratio1 `bimult` y2)
    y1 =! (x1 `bidiv` ratio2)
    y2 =! (x2 `bidiv` ratio1)

--main :: IO ()
--main = do
--    (buildingHeight :: Cell (Range Rational))
--        <- quiesce $ do
--        barometerHeight <- emptyCell
--        barometerShadow <- emptyCell
--        buildingHeight <- emptyCell
--        buildingShadow <- emptyCell
--        similarTriangles (barometerShadow, barometerHeight) (buildingShadow, buildingHeight)
--        constant ("shadow" `implies` Range 54.9 55.1) buildingShadow
--        constant ("blueprints" `implies` Range 54.8 55) buildingShadow
--        constant ("shadow" `implies` Range 0.3 0.32) barometerHeight
--        -- constant ("measurement" `implies` Range 0.31 0.32) barometerHeight
--        constant ("shadow" `implies` Range 0.36 0.37) barometerShadow
--        constant ("pressure" `implies` Range 46 50) buildingHeight
--        constant ("superindendent" `implies` Range 45 45) buildingHeight
--        return buildingHeight
--    height <- readCell buildingHeight
--    let floatHeight = (fmap . fmap) (fromRational @Double) height
--    -- print (fmap (fromRational @Double) <$> showEvidence height)
--    putStrLn $ showAllEvidence . evidenceWithout "pressure" $ floatHeight
--    putStrLn $ showBestEvidence . evidenceWithout "pressure" $ floatHeight
--    -- print $ height
--    --

main :: IO ()
main = do
    (buildingHeight :: Cell (Range Double))
        <- quiesce $ do
        barometerHeight <- emptyCell
        barometerShadow <- emptyCell
        buildingHeight <- emptyCell
        buildingShadow <- emptyCell
        similarTriangles (barometerShadow, barometerHeight) (buildingShadow, buildingHeight)
        constant (Range 54.9 55.1) buildingShadow
        constant (Range 54.8 55) buildingShadow
        constant (Range 0.3 0.32) barometerHeight
        -- constant (Range 0.31 0.32) barometerHeight
        constant (Range 0.36 0.37) barometerShadow
        constant (Range 46 50) buildingHeight
        constant (Range 45 45) buildingHeight
        return buildingHeight
    height <- readCell buildingHeight
    print height
