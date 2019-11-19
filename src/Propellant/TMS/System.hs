{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Propellant.TMS.System where

import Propellant.Prop
import Control.Concurrent.STM
import qualified Data.Set as S
import Propellant.Supported
import Propellant.Scheduler
import Propellant.Cell
import Control.Monad.Reader
import Propellant.Merge

data Registry e = Registry {banned :: (S.Set e)}
data System e = System {scheduler :: Scheduler, registry :: Cell (Registry e)}

data TCell e a = TCell (Cell (TMS e a))

newtype TBuilder e a = TBuilder {runTBuilder :: ReaderT (System e) STM a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (System e))

instance HasSTM (TBuilder e) where
  liftSTM = TBuilder . lift

instance HasScheduler (TBuilder e) where
  schedule p = do
      sched <- asks scheduler
      liftSTM $ writeTQueue sched p

watchRegistry :: forall e a. (Ord e, Ord a) => TCell e a -> TBuilder e ()
watchRegistry (TCell c) = do
    reg <- asks registry
    addNeighbour reg (p reg)
  where
    p :: Cell (Registry e) -> Propagator
    p reg = do
      r <- contents reg
      case r of
          Nothing -> error "empty registry!"
          Just (Registry bans) -> addContent (TMS mempty (Just bans)) c

newTCell :: (Ord e, Ord a, Mergeable a) => e -> a -> TBuilder e (TCell e a)
newTCell e a = do
    c <- TCell <$> newCell (singletonTMS $ e `supports` a)
    watchRegistry c
    return c

tcontents :: (Mergeable a, HasSTM m, Ord e) => TCell e a -> m (Maybe (Supported e a))
tcontents (TCell c) = do
    contents c >>= \case
      Nothing -> return Nothing
      Just tms -> return $ mostInformative tms

kickOut :: (HasSTM m, HasScheduler m, Ord e) => Cell (Registry e) -> e -> m ()
kickOut cell@(Cell c _ _) e = do
    liftSTM $ do
        readTVar c >>= \case
          Just (Registry r) -> writeTVar c (Just . Registry $ S.insert e r)
          Nothing -> error "empty registry"
    propagate cell

bringIn :: (HasSTM m, HasScheduler m, Ord e) => Cell (Registry e) -> e -> m ()
bringIn cell@(Cell c _ _) e = do
    liftSTM $ do
        readTVar c >>= \case
          Just (Registry r) -> writeTVar c (Just . Registry $ S.delete e r)
          Nothing -> error "empty registry"
    propagate cell
