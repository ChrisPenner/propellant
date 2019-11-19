{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Propellant.Cell where

import Propellant.Prop
import Propellant.Supported
import Propellant.Merge
import Propellant.Scheduler
import Control.Concurrent.STM
import Data.Foldable
import Data.Maybe
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Debug.Trace

data Cell e a where
    Cell :: (Mergeable a, Ord e, Eq a) => { cellContent    :: TVar (TMS e a)
            , cellDependents :: TVar [Propagator]
            } -> Cell e a

-- | Pointer equality
instance Eq (Cell e a) where
  Cell c _ == Cell c' _ = c == c'

contents :: HasSTM m => Cell e a -> m (TMS e a)
contents c@Cell{} = liftSTM . readTVar . cellContent $ c

constructCell :: (HasSTM m, Mergeable a, Ord e, Ord a) => Maybe (Supported e a) -> m (Cell e a)
constructCell a = Cell <$> liftSTM (newTVar . fromMaybe (TMS mempty) $ fmap singletonTMS a) <*> liftSTM (newTVar [])

newCell :: (HasSTM m, Mergeable a, Ord e, Ord a) => Supported e a ->  m (Cell e a)
newCell = constructCell . Just

emptyCell :: (HasSTM m, Mergeable a, Ord e, Ord a) => m (Cell e a)
emptyCell = constructCell Nothing

readCell :: Cell e a -> IO (TMS e a)
readCell = contents

addContent :: (HasSTM m, HasScheduler m) => TMS e a -> Cell e a -> m ()
addContent new cell@(Cell c _) = do
    before <- liftSTM $ readTVar c
    let merged = merge new before
    case merged of
        NoChange a -> liftSTM $ writeTVar c a
        Changed s a -> do
            liftSTM $ writeTVar c a
            recordInvalids s
            propagate cell
        Contradiction -> error "contradiction!"

recordInvalids :: Monad m => S.Set (IS.IntSet) -> m ()
recordInvalids s = traceShowM s

propagate :: (HasSTM m, HasScheduler m) => Cell e a -> m ()
propagate (Cell _ depsT) = do
    deps <- liftSTM $ readTVar depsT
    for_ deps schedule

addNeighbour :: (HasScheduler m, HasSTM m) => Cell e a -> Propagator -> m ()
addNeighbour cell prop  = do
    liftSTM $ modifyTVar (cellDependents cell) (prop:)
    schedule prop

using :: Cell e a -> (TMS e a -> Propagator) -> Propagator
using c f = contents c >>= f

-- usingWhen :: Cell e a -> (a -> Bool) -> (a -> Propagator) -> Propagator
-- usingWhen c predicate f =
--     using c $ \a -> if predicate a
--         then f a
--         else return ()
