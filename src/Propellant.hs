{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
module Propellant where

import Control.Concurrent.STM
import Control.Lens
import Algebra.Lattice
import Control.Monad.Reader
import Data.Foldable
import Control.Concurrent
import Debug.Trace

data Cell i =
    Cell { cellContent    :: TVar i
         , cellDependents :: TVar [Propagator]
         }

type Info i = (BoundedJoinSemiLattice i, Eq i)
type Scheduler = TQueue Propagator
type Prop a = ReaderT Scheduler STM a
data Propagator = Propagator {label :: [String], runPropagator :: Prop ()}

instance Semigroup Propagator where
  Propagator l a <> Propagator l' b = Propagator (l <> l') (a *> b)

instance Monoid Propagator where
  mempty = Propagator [] (return ())

makeLenses ''Cell

contents :: Cell i -> STM i
contents (Cell iT _) = readTVar iT

newCell :: i -> IO (Cell i)
newCell i = Cell <$> newTVarIO i <*> newTVarIO []

readCell :: Cell i -> IO i
readCell = readTVarIO . cellContent

addContent :: (Info i) => i -> Cell i -> Propagator
addContent info cell@(Cell c _) = Propagator ["add content"] $ do
    before <- lift $ readTVar c
    let after = before \/ info
    when (before /= after) $ do
        lift $ writeTVar c after
        propagate cell

propagate :: Cell i -> Prop ()
propagate (Cell _ depsT) = do
    deps <- lift $ readTVar depsT
    for_ deps schedule

schedule :: Propagator -> Prop ()
schedule m = do
    sched <- ask
    lift $ writeTQueue sched m

addNeighbour :: Cell i -> Propagator -> Prop ()
addNeighbour cell prop  = do
    lift $ modifyTVar (cellDependents cell) (prop:)
    schedule prop

quiesce :: Propagator -> IO ()
quiesce (Propagator msg m) = do
    sched <- newScheduler
    atomically . flip runReaderT sched $ m
    -- print msg
    drain sched

data Done = Done | NotDone
drain :: Scheduler -> IO ()
drain sched = do
    -- print "drain"
    r <- do
        atomically $ do
        tryReadTQueue sched >>= \case
            Nothing -> return ([], Done)
            Just propagator -> do
                runReaderT (runPropagator propagator) sched
                return (label propagator, NotDone)
    case r of
        (lab, NotDone) -> do
            -- print lab
            drain sched
        (_, Done) -> return ()

newScheduler :: IO Scheduler
newScheduler = newTQueueIO
