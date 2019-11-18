module Examples.Choice where

import Propellant
import Control.Applicative

main :: IO ()
main = do
    (cellA, cellB, outCell) <- quiesce $ do
        cellA <- oneOf [("a1", 1 :: Int), ("a2", 2)]
        cellB <- oneOf [("b1", 1), ("b2", 2)]
        out <- store (cellA `bisum` cellB)
        constant ("fact" `implies` 3) out
        notifier out
        notifier cellA
        notifier cellB
        return (cellA, cellB, out)
    a <- readCell cellA
    b <- readCell cellB
    out <- readCell outCell
    -- print a
    -- print b
    print out
    return ()


-- equal :: (Ord e, Eq a) => Cell (Evidence e a) -> Cell (Evidence e a) -> Cell (Evidence e Bool) -> Builder ()
-- equal a b c = liftP2 (liftA2 (==)) a b c

        -- aEqB =! (cellA =? cellB)
        -- -- constant' (3 :: Wide Int) cellA
        -- constant' 4 cellB
        -- constant' (pure True :: Wide Bool) aEqB
        -- return (cellA, cellB, aEqB)
    -- a :: (Evidence String (Wide Int)) <- readCell cellA
    -- b :: (Evidence String (Wide Int)) <- readCell cellB
    -- areEq <- readCell aEqB
    -- putStrLn $ "A"
    -- putStrLn $ showAllEvidence a
    -- putStrLn $ "B"
    -- putStrLn $ showAllEvidence b
    -- putStrLn $ "A = B"
    -- putStrLn $ showAllEvidence areEq

