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
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as M
import qualified Data.IntSet as IS
import Data.Functor.Apply

data Supported e a = Supported {buckets :: (S.Set e), choices :: IS.IntSet, value :: a}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Ord e) => Applicative (Supported e) where
  pure = Supported mempty mempty
  Supported e c f <*> Supported e' c' a   = Supported (e <> e') (c <> c') (f a)

instance (Ord e) => Monad (Supported e) where
  return = pure
  Supported e c a >>= f =
      case f a of
          Supported e' c' x -> Supported (e <> e') (c <> c') x

instance (Mergeable a, Ord e) => Mergeable (Supported e a) where
  merge new@(Supported e' c' a') existing@(Supported e c a) =
      case (merge a' a, merge a a') of
          (NoChange _, _) -> NoChange existing
          (_, NoChange _) -> NoChange new
          (Changed s result, _) -> Changed s (Supported (e <> e') (c <> c') result)
          (_, Changed s result) -> Changed s (Supported (e <> e') (c <> c') result)
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

supports :: e -> Int -> a -> Supported e a
supports e c a = Supported (S.singleton e) (IS.singleton c) a

-------------

data TMS e a = TMS (M.Map (S.Set e) (M.Map IS.IntSet a))
  deriving (Show, Eq, Functor)

instance Apply (TMS e) where
  (<.>) (TMS fs) (TMS as) = do
      (e, f) <- M.toList fs
      (e', a) <- M.toList as
      let paired = M.

-- mapTMS :: (Ord e, Ord b) => (a -> b) -> TMS e a -> TMS e b
-- mapTMS f (TMS m) = TMS (S.map (fmap f) e) b

-- liftT2 :: (a -> b -> c) -> TMS e a -> TMS e b -> TMS e c
-- liftT2 =


singletonTMS :: Supported e a -> TMS e a
singletonTMS s = TMS (M.singleton (buckets s) (M.singleton (choices s) (value s)))

-- addSupport :: forall e a. (Mergeable a, Ord e, Ord a) => Supported e a -> TMS e a -> TMS e a
-- addSupport new@(Supported es c _) (TMS m) =
--     let (result, Any alreadySubsumed) = foldMap go (toList xs)
--         claims = if alreadySubsumed then result
--                                     else S.insert new result
--      in TMS claims bans
--   where
--     go :: Supported e a -> (S.Set (Supported e a), Any)
--     go existing@(Supported es' _) =
--         -- If deps are supports are equal we can just merge
--         if | es == es' -> (foldMap  S.singleton (merge new existing ), Any True)
--            | new `subsumes` existing -> (S.singleton new, Any True)
--            | existing `subsumes` new -> (S.singleton existing, Any True)
--            | otherwise -> (S.singleton existing, Any False)

-- subsumes :: Mergeable a => (Ord e) => Supported e a -> Supported e a -> Bool
-- subsumes (Supported e a) (Supported e' a')
--   = e `S.isSubsetOf` e' && a' `leq` a

-- leq :: Mergeable a => a -> a -> Bool
-- leq a b = case merge b a of
--     NoChange _ -> True
--     Changed _ -> False
--     Contradiction -> False

-- instance (Mergeable a, Ord e, Ord a) => Semigroup (TMS e a) where
--   TMS a bans <> b =
--       let TMS newClaims bans' = S.foldl' (flip addSupport) b a
--        in TMS newClaims (bans <> bans')

-- instance (Mergeable a, Ord e, Ord a) => Monoid (TMS e a) where
--   mempty = TMS mempty Nothing

instance (Mergeable a, Ord e) => Mergeable (TMS e a) where
  merge (TMS new) (TMS existing) =
      case (M.mergeA M.preserveMissing M.preserveMissing (M.zipWithAMatched combineM) new existing) of
          ((Any True, s), a) -> Changed s (TMS a)
          ((Any False, _), a) -> NoChange (TMS a)
          -- TODO Handle contradiction here
    where
      combine k a b =
          case merge a b of
              NoChange r -> pure (Just r)
              Changed s r -> ((Any True, s), Just r)
              Contradiction -> ((Any True, S.singleton k), Nothing)
      combineM _ a b =
          M.mergeA M.preserveMissing M.preserveMissing (M.zipWithMaybeAMatched combine) a b

-- isContradiction :: Merged -> Bool
-- isContradiction Contradiction{} = True
-- isContradiction _ = False

-- mostInformative :: forall e a. (Mergeable a, Ord e) => TMS e a -> Maybe (Supported e a)
-- mostInformative (TMS m _) =
--     (maximumByOf traversed compareDefinedness . concatMap toList . fmap combine . toList $ S.powerSet m)
--   where
--     combine :: S.Set (Supported e a) -> Merged (Supported e a)
--     combine (toList -> (x : xs)) = foldM (flip merge) x xs
--     -- Empty set
--     combine _ = Contradiction

-- compareDefinedness :: (Mergeable a) => (Supported e a) -> (Supported e a) -> Ordering
-- compareDefinedness (Supported e a) (Supported e' b)
--     -- if data is equal, the fewer deps is preferred
--     | a `leq` b && b `leq` a = compare (length e') (length e)
--     | a `leq` b = LT
--     | b `leq` a = GT
--     | otherwise = compare (length e') (length e)

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
