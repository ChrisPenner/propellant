{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Propellant.Cell where

import Propellant.Prop
import Propellant.Merge
import Propellant.Scheduler
import Control.Concurrent.STM
import Control.Applicative
import Data.Foldable

data Cell a where
    Cell :: { cellContent    :: TVar (Maybe a)
            , cellDependents :: TVar [Propagator]
            , cellMerge :: a -> a -> Merged a
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
    let merged = liftA2 merge' before (Just new)
    case merged of
        Nothing -> do
            liftSTM $ writeTVar c (Just new)
            propagate cell
        Just (Changed a) -> do
            liftSTM $ writeTVar c (Just a)
            propagate cell
        Just (NoChange a) -> liftSTM $ writeTVar c (Just a)
        Just (Contradiction) -> fail "contradiction!"

propagate :: Cell a -> Propagator
propagate (Cell _ depsT _) = do
    deps <- liftSTM $ readTVar depsT
    for_ deps schedule

addNeighbour :: Cell a -> Propagator -> Builder ()
addNeighbour cell prop  = do
    liftSTM $ modifyTVar (cellDependents cell) (prop:)
    schedule prop

onUpdate :: [Cell a] -> Propagator -> Builder ()
onUpdate cells p = for_ cells (flip addNeighbour p)

using :: Cell a -> (a -> Propagator) -> Propagator
using c f = contents c >>= \case
  Nothing -> return ()
  Just a -> f a
