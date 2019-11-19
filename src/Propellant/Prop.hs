{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Propellant.Prop where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Applicative

type Propagator = Prop ()
type Scheduler = TQueue Propagator

newtype Builder a = Builder {runBuilder :: ReaderT Scheduler STM a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Scheduler)

newtype Prop a = Prop {runProp :: ReaderT Scheduler STM a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Scheduler)

instance Semigroup s => Semigroup (Prop s) where
  (<>) = liftA2 (<>)

instance Monoid m => Monoid (Prop m) where
  mempty = Prop (return mempty)

class Monad m => HasSTM m where
  liftSTM :: STM a -> m a

instance HasSTM IO where
  liftSTM = atomically

instance HasSTM Builder where
  liftSTM = Builder . lift

instance HasSTM Prop where
  liftSTM = Prop . lift
