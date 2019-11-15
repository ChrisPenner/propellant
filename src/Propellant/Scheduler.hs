{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Propellant.Scheduler where

import Control.Concurrent.STM
import Control.Monad.Reader
import Propellant.Prop

quiesce :: Builder a -> IO a
quiesce (Builder m) = do
    sched <- newScheduler
    a <- atomically . flip runReaderT sched $ m
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

class HasScheduler m where
  schedule :: Propagator -> m ()
  default schedule :: (HasSTM m, MonadReader Scheduler m) => Propagator -> m ()
  schedule p = do
      sched <- ask
      liftSTM $ writeTQueue sched p

instance HasScheduler Prop
instance HasScheduler Builder
