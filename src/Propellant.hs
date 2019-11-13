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
newtype Builder a = Builder {runBuilder :: ReaderT Scheduler STM a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Scheduler)

newtype Prop a = Prop {runProp :: ReaderT Scheduler STM a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Scheduler)

type Propagator = Prop ()

instance Semigroup s => Semigroup (Prop s) where
  (<>) = liftA2 (<>)

instance Monoid m => Monoid (Prop m) where
  mempty = Prop (return mempty)

makeLenses ''Cell

contents :: Cell i -> Prop i
contents (Cell iT _) = liftSTMP $ readTVar iT

-- newCell :: i -> IO (Cell i)
-- newCell i = Cell <$> newTVarIO i <*> newTVarIO []

newCell :: i -> Builder (Cell i)
newCell i = Cell <$> liftSTMB (newTVar i) <*> liftSTMB (newTVar [])

emptyCell :: Info i => Builder (Cell i)
emptyCell = newCell bottom

newCellT :: i -> STM (Cell i)
newCellT i = Cell <$> newTVar i <*> newTVar []

readCell :: Cell i -> IO i
readCell = readTVarIO . cellContent

readCellT :: Cell i -> STM i
readCellT = readTVar . cellContent

liftSTMP :: STM a -> Prop a
liftSTMP = Prop . lift

liftSTMB :: STM a -> Builder a
liftSTMB = Builder . lift


addContent :: (Info i) => i -> Cell i -> Propagator
addContent info cell@(Cell c _) = do
    before <- liftSTMP $ readTVar c
    let after = before \/ info
    when (before /= after) $ do
        liftSTMP $ writeTVar c after
        propagate cell

propagate :: Cell i -> Propagator
propagate (Cell _ depsT) = do
    deps <- liftSTMP $ readTVar depsT
    for_ deps (Prop . schedule')

scheduleB :: Propagator -> Builder ()
scheduleB = Builder . schedule'

schedule' :: Propagator -> ReaderT Scheduler STM ()
schedule' m = do
    sched <- ask
    lift $ writeTQueue sched m

addNeighbour :: Cell i -> Propagator -> Builder ()
addNeighbour cell prop  = do
    Builder . lift $ modifyTVar (cellDependents cell) (prop:)
    scheduleB prop

quiesce :: Builder a -> IO a
quiesce (Builder m) = do
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
