{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Propellant.Guesser where

import Propellant.Supports
import Propellant.Cell
import Propellant.Prop
import Propellant.Merge
import Propellant.Scheduler
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Data.List
import Data.Function
import Data.Functor.Compose
import Data.Foldable
import qualified Data.Set as S
import Data.Functor

oneOf :: forall f a e. (Ord e, Foldable f, Eq a, Mergeable a) => f (e, a) -> Builder (Cell (Evidence e a))
oneOf (toList -> as) = do
    container <- emptyCell
    let supports :: [(S.Set (Support e), Merged a)] =
          as <&> \(e, a) ->
            let hypo = Hypothetical e cancel
                cancel = addContent (Evidence (M.singleton (S.singleton hypo) Contradiction)) container
            in (S.singleton hypo, NoChange a)
    schedule $ addContent (Evidence . M.fromList $ supports) container
    output <- emptyCell
    chooser container output
    return output

chooser :: Cell (Evidence e a) -> Cell (Evidence e a) -> Builder ()
chooser inA out = do
    addNeighbour inA p
  where
    p = do
        using inA $ \(Evidence m) -> do
        let xs = do
            (k, v) <- M.toList $ m
            case v of
                Contradiction -> empty
                NoChange a -> return (k, a)
                Changed a -> return (k, a)
        when (not $ null xs) $ do
        let (supports, choice) = minimumBy (compare `on` length . fst) xs
        addContent (supports `implies'` choice) out

notifier :: Cell (Evidence e a) -> Builder ()
notifier cell = do
    addNeighbour cell p
  where
    anyValid (Evidence m) = null $ Compose m
    allContradictions (Evidence m) = filter (isContradiction . snd) . M.toList $ m
    isContradiction Contradiction{} = True
    isContradiction _ = False
    p = do
      using cell $ \a -> do
          when (not (anyValid a)) $ do
              let contradictions = allContradictions a
              for_ contradictions $ \(supports, _) -> do
                  for_ supports $ \s -> do
                      reject s

reject :: Support a -> Propagator
reject (Hypothetical _ p) = p
reject _ = return ()
