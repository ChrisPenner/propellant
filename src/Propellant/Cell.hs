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

contents :: Cell a -> Prop (Maybe a)
contents  = liftSTM . readTVar . cellContent

constructCell :: Mergeable a => Maybe a ->  Builder (Cell a)
constructCell a = Cell <$> liftSTM (newTVar $ a) <*> liftSTM (newTVar []) <*> pure merge

newCell :: Mergeable a => a ->  Builder (Cell a)
newCell = constructCell . Just

emptyCell :: Mergeable a => Builder (Cell a)
emptyCell = constructCell Nothing

readCell :: Cell a -> IO (Maybe a)
readCell = readTVarIO . cellContent

addContent :: a -> Cell a -> Propagator
addContent new cell@(Cell c _ merge') = do
    before <- liftSTM $ readTVar c
    let merged = merge' before (Just new)
    case merged of
        NoChange a -> liftSTM $ writeTVar c a
        Changed a -> do
            liftSTM $ writeTVar c a
            propagate cell
        Contradiction -> error "contradiction!"

propagate :: Cell a -> Propagator
propagate (Cell _ depsT _) = do
    deps <- liftSTM $ readTVar depsT
    for_ deps schedule

addNeighbour :: Cell a -> Propagator -> Builder ()
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
