{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Propellant where

import Control.Concurrent.STM
import Control.Lens
import Algebra.Lattice
import Control.Monad.Reader
import Control.Applicative
import Data.Foldable

data Cell i =
    Cell { cellContent    :: TVar i
         , cellDependents :: TVar [Propagator]
         }

type Info i = (BoundedJoinSemiLattice i, Eq i)
type Scheduler = TQueue Propagator
newtype Prop a = Prop {runProp :: ReaderT Scheduler STM a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Scheduler)

type Propagator = Prop ()

instance Semigroup s => Semigroup (Prop s) where
  (<>) = liftA2 (<>)

instance Monoid m => Monoid (Prop m) where
  mempty = Prop (return mempty)

makeLenses ''Cell

contents :: Cell i -> STM i
contents (Cell iT _) = readTVar iT

-- newCell :: i -> IO (Cell i)
-- newCell i = Cell <$> newTVarIO i <*> newTVarIO []

newCell :: i -> Prop (Cell i)
newCell i = Cell <$> liftSTM (newTVar i) <*> liftSTM (newTVar [])

newCellT :: i -> STM (Cell i)
newCellT i = Cell <$> newTVar i <*> newTVar []

readCell :: Cell i -> IO i
readCell = readTVarIO . cellContent

readCellT :: Cell i -> STM i
readCellT = readTVar . cellContent

liftSTM :: STM a -> Prop a
liftSTM = Prop . lift

addContent :: (Info i) => i -> Cell i -> Propagator
addContent info cell@(Cell c _) = do
    before <- liftSTM $ readTVar c
    let after = before \/ info
    when (before /= after) $ do
        liftSTM $ writeTVar c after
        propagate cell

propagate :: Cell i -> Prop ()
propagate (Cell _ depsT) = do
    deps <- liftSTM $ readTVar depsT
    for_ deps schedule

schedule :: Propagator -> Prop ()
schedule m = do
    sched <- ask
    liftSTM $ writeTQueue sched m

addNeighbour :: Cell i -> Propagator -> Prop ()
addNeighbour cell prop  = do
    liftSTM $ modifyTVar (cellDependents cell) (prop:)
    schedule prop

quiesce :: Prop a -> IO a
quiesce (Prop m) = do
    sched <- newScheduler
    a <- atomically . flip runReaderT sched $ m
    -- print msg
    drain sched
    return a

data Done = Done | NotDone
drain :: Scheduler -> IO ()
drain sched = do
    -- print "drain"
    r <- do
        atomically $ do
        tryReadTQueue sched >>= \case
            Nothing -> return Done
            Just propagator -> do
                runReaderT (runProp propagator) sched
                return NotDone
    case r of
        NotDone -> do
            -- print lab
            drain sched
        Done -> return ()

newScheduler :: IO Scheduler
newScheduler = newTQueueIO
