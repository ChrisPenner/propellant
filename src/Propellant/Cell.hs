{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Propellant.Cell where

import Propellant.Prop
import Propellant.Merge
import Propellant.Scheduler
import Control.Concurrent.STM
import Data.Foldable

data Cell a where
    Cell :: { cellContent    :: TVar (Maybe a)
            , cellDependents :: TVar [Propagator]
            , cellMerge :: Maybe a -> Maybe a -> Merged (Maybe a)
            } -> Cell a

-- | Pointer equality
instance Eq (Cell a) where
  Cell c _ _ == Cell c' _ _ = c == c'

contents :: HasSTM m => Cell a -> m (Maybe a)
contents  = liftSTM . readTVar . cellContent

constructCell :: (HasSTM m, Mergeable a) => Maybe a -> m (Cell a)
constructCell a = Cell <$> liftSTM (newTVar $ a) <*> liftSTM (newTVar []) <*> pure merge

newCell :: (HasSTM m, Mergeable a) => a ->  m (Cell a)
newCell = constructCell . Just

emptyCell :: (HasSTM m, Mergeable a) => m (Cell a)
emptyCell = constructCell Nothing

readCell :: Cell a -> IO (Maybe a)
readCell = readTVarIO . cellContent

addContent :: (HasSTM m, HasScheduler m) => a -> Cell a -> m ()
addContent new cell@(Cell c _ merge') = do
    before <- liftSTM $ readTVar c
    let merged = merge' (Just new) before
    case merged of
        NoChange a -> liftSTM $ writeTVar c a
        Changed a -> do
            liftSTM $ writeTVar c a
            propagate cell
        Contradiction -> error "contradiction!"

propagate :: (HasSTM m, HasScheduler m) => Cell a -> m ()
propagate (Cell _ depsT _) = do
    deps <- liftSTM $ readTVar depsT
    for_ deps schedule

addNeighbour :: (HasScheduler m, HasSTM m) => Cell a -> Propagator -> m ()
addNeighbour cell prop  = do
    liftSTM $ modifyTVar (cellDependents cell) (prop:)
    schedule prop

using :: Cell a -> (a -> Propagator) -> Propagator
using c f = contents c >>= \case
  Nothing -> return ()
  Just a -> f a

usingWhen :: Cell a -> (a -> Bool) -> (a -> Propagator) -> Propagator
usingWhen c predicate f =
    using c $ \a -> if predicate a
        then f a
        else return ()
