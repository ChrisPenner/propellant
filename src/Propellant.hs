{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Propellant where

import Control.Concurrent.STM
import Control.Lens
import Algebra.Lattice
import Control.Monad.Reader
import Control.Applicative
import Data.Foldable
import Propellant.Lattices.Evidence

data Cell e i where
    Cell :: Info e i
         => { cellContent    :: TVar (Evidence e i)
            , cellDependents :: TVar [Propagator]
            } -> Cell e i

-- | Pointer equality
instance Eq (Cell e i) where
  Cell c _ == Cell c' _ = c == c'

type Info e i = (Ord e, Eq i, Lattice i)
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

contents :: Cell e i -> Prop (Evidence e i)
contents  = liftSTMP . readTVar . cellContent

-- newCell :: i -> IO (Cell e i)
-- newCell i = Cell e <$> newTVarIO i <*> newTVarIO []

newCell :: Info e i => Evidence e i -> Builder (Cell e i)
newCell i = Cell <$> liftSTMB (newTVar i) <*> liftSTMB (newTVar [])

emptyCell :: Info e i => Builder (Cell e i)
emptyCell = newCell bottom

-- newCellT :: _ => i -> STM (Cell e i)
-- newCellT i = Cell <$> newTVar i <*> newTVar []

readCell :: Cell e i -> IO (Evidence e i)
readCell = readTVarIO . cellContent

-- readCellT :: Cell e i -> STM i
-- readCellT = readTVar . cellContent

liftSTMP :: STM a -> Prop a
liftSTMP = Prop . lift

liftSTMB :: STM a -> Builder a
liftSTMB = Builder . lift


addContent :: Evidence e i -> Cell e i -> Propagator
addContent info cell@(Cell c _) = do
    before <- liftSTMP $ readTVar c
    let after = before \/ info
    when (before /= after) $ do
        liftSTMP $ writeTVar c after
        propagate cell

propagate :: Cell e i -> Propagator
propagate (Cell _ depsT) = do
    deps <- liftSTMP $ readTVar depsT
    for_ deps (Prop . schedule')

scheduleB :: Propagator -> Builder ()
scheduleB = Builder . schedule'

schedule' :: Propagator -> ReaderT Scheduler STM ()
schedule' m = do
    sched <- ask
    lift $ writeTQueue sched m

addNeighbour :: Cell e i -> Propagator -> Builder ()
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
