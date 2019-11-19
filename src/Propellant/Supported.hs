{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
module Propellant.Supported where

import qualified Data.Set as S
import Propellant.Merge
import Control.Applicative
import Data.Foldable
import Data.Monoid
import Control.Monad
import Control.Lens

data Supported e a = Supported (S.Set e) a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Ord e) => Applicative (Supported e) where
  pure = Supported mempty
  Supported e f <*> Supported e' a   = Supported (e <> e') (f a)

instance (Ord e) => Monad (Supported e) where
  return = pure
  Supported e a >>= f =
      case f a of
          Supported e' x -> Supported (e <> e') x

instance (Mergeable a, Ord e) => Mergeable (Supported e a) where
  merge new@(Supported e' a') existing@(Supported e a) =
      case (merge a' a, merge a a') of
          (NoChange _, _) -> NoChange existing
          (_, NoChange _) -> NoChange new
          (Changed result, _) -> Changed (Supported (e <> e') result)
          (_, Changed result) -> Changed (Supported (e <> e') result)
          (Contradiction, _) -> Contradiction

instance (Ord e, Monoid e, Num n) => Num (Supported e n) where
  a + b = liftA2 (+) a b
  a - b = liftA2 (-) a b
  a * b = liftA2 (*) a b
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Ord e, Monoid e, Fractional n) => Fractional (Supported e n) where
  fromRational = pure . fromRational
  recip = fmap recip

supports :: e -> a -> Supported e a
supports e a = Supported (S.singleton e) a

-------------

data TMS e a = TMS { knowledge :: (S.Set (Supported e a))
                   , banned :: Maybe (S.Set e)}
  deriving (Show, Eq)

singletonTMS :: Supported e a -> TMS e a
singletonTMS s = TMS (S.singleton s) Nothing

addSupport :: forall e a. (Mergeable a, Ord e, Ord a) => Supported e a -> TMS e a -> TMS e a
addSupport new@(Supported es _) (TMS xs bans) =
    let (result, Any alreadySubsumed) = foldMap go (toList xs)
        claims = if alreadySubsumed then result
                                    else S.insert new result
     in TMS claims bans
  where
    go :: Supported e a -> (S.Set (Supported e a), Any)
    go existing@(Supported es' _) =
        -- If deps are supports are equal we can just merge
        if | es == es' -> (foldMap  S.singleton (merge new existing ), Any True)
           | new `subsumes` existing -> (S.singleton new, Any True)
           | existing `subsumes` new -> (S.singleton existing, Any True)
           | otherwise -> (S.singleton existing, Any False)

subsumes :: Mergeable a => (Ord e) => Supported e a -> Supported e a -> Bool
subsumes (Supported e a) (Supported e' a')
  = e `S.isSubsetOf` e' && a' `leq` a

leq :: Mergeable a => a -> a -> Bool
leq a b = case merge b a of
    NoChange _ -> True
    Changed _ -> False
    Contradiction -> False

instance (Mergeable a, Ord e, Ord a) => Semigroup (TMS e a) where
  TMS a bans <> b =
      let TMS newClaims bans' = S.foldl' (flip addSupport) b a
       in TMS newClaims (bans <> bans')

instance (Mergeable a, Ord e, Ord a) => Monoid (TMS e a) where
  mempty = TMS mempty Nothing

instance (Mergeable a, Ord e, Ord a) => Mergeable (TMS e a) where
  merge new existing =
      let result = existing <> new
       in if result == existing then NoChange result
                                else Changed result

-- isContradiction :: Merged -> Bool
-- isContradiction Contradiction{} = True
-- isContradiction _ = False

mostInformative :: forall e a. (Mergeable a, Ord e) => TMS e a -> Maybe (Supported e a)
mostInformative (TMS m _) =
    (maximumByOf traversed compareDefinedness . concatMap toList . fmap combine . toList $ S.powerSet m)
  where
    combine :: S.Set (Supported e a) -> Merged (Supported e a)
    combine (toList -> (x : xs)) = foldM (flip merge) x xs
    -- Empty set
    combine _ = Contradiction

compareDefinedness :: (Mergeable a) => (Supported e a) -> (Supported e a) -> Ordering
compareDefinedness (Supported e a) (Supported e' b)
    -- if data is equal, the fewer deps is preferred
    | a `leq` b && b `leq` a = compare (length e') (length e)
    | a `leq` b = LT
    | b `leq` a = GT
    | otherwise = compare (length e') (length e)

-- one, two, three :: Supported String (Interval Int)
-- one = "one" `supports` (I 1 10)
-- two = "two" `supports` (I 3 12)
-- three = "three" `supports` (I 4 6)
-- -- four = "one" `supports` (I 5 6)
-- -- test :: TMS String (Interval Int)
-- test =  foldMap (TMS . S.singleton)
--   [
--   -- , one
--   -- , two
--   -- , three
--     (Supported (S.fromList ["one"]) (I 4 6))
--   , (Supported (S.fromList ["one", "two"]) (I 5 6))
--   , (Supported (S.fromList ["six"]) (I 100 101))
--   ]
-- -- expected:
-- -- >>> test
-- -- (fromList ["four","three"],Range {rangeMin = 4, rangeMax = 8})
