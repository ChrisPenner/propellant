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
  merge before@(Supported e a) new@(Supported e' a') =
      case (merge a a', merge a' a) of
          (NoChange _, _) -> NoChange before
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

data TMS e a = TMS (S.Set (Supported e a))
  deriving (Show, Eq)

addSupport :: forall e a. (Mergeable a, Ord e, Ord a) => Supported e a -> TMS e a -> TMS e a
addSupport new@(Supported es _) (TMS xs) =
    let (result, Any alreadySubsumed) = foldMap go (toList xs)
     in TMS $ if alreadySubsumed then result
                                 else S.insert new result
  where
    go :: Supported e a -> (S.Set (Supported e a), Any)
    go existing@(Supported es' _) =
        -- If deps are supports are equal we can just merge
        if | es == es' -> (foldMap  S.singleton (merge existing new), Any True)
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
  TMS a <> b = S.foldl' (flip addSupport) b a

instance (Mergeable a, Ord e, Ord a) => Monoid (TMS e a) where
  mempty = TMS mempty

instance (Mergeable a, Ord e, Ord a) => Mergeable (TMS e a) where
  merge before new =
      let result = before <> new
       in if result == before then NoChange result
                              else Changed result

-- isContradiction :: Merged -> Bool
-- isContradiction Contradiction{} = True
-- isContradiction _ = False

-- TODO: make this not partial
mostInformative :: forall e a. (Mergeable a, Ord e) => TMS e a -> (Supported e a)
mostInformative (TMS m) =
    (maximumBy go . concatMap toList . fmap combine . toList $ S.powerSet m)
  where
    combine :: S.Set (Supported e a) -> Merged (Supported e a)
    combine (toList -> (x : xs)) = foldM merge x xs
    -- Empty set
    combine _ = Contradiction
    go :: (Supported e a) -> (Supported e a) -> Ordering
    go (Supported _ a) (Supported _ b)
      | a `leq` b && b `leq` a = EQ
      | a `leq` b = LT
      | otherwise = GT

-- instance (Ord e, Mergeable a) => Semigroup (TMS e a) where
--   a <> b = superSet $ accumulateEvidence a b

-- accumulateEvidence :: forall e v. (Ord e, Mergeable v) => TMS e v -> TMS e v -> TMS e v
-- accumulateEvidence (TMS a) (TMS b) = TMS combined -- clean combined
--   where
--     combined = M.unionWith go a b
--     go :: Merged v -> Merged v -> Merged v
--     go mx my = do
--         x <- mx
--         y <- my
--         merge x y

-- superSet :: (Ord e, Mergeable v) => TMS e v -> TMS e v
-- superSet (TMS m) = TMS . M.fromList $ do
--     (k, v) <- M.toList m
--     (k', v') <- M.toList m
--     case join (liftA2 merge v v') of
--         Contradiction -> empty
--         a -> return (k <> k', a)

-- instance (Ord e, Mergeable a) => Monoid (TMS e a) where
--   mempty = TMS mempty

-- instance (Ord e) => Applicative (TMS e) where
--   pure v = TMS (M.singleton mempty (pure v))
--   (<*>) :: forall a b. TMS e (a -> b) -> TMS e a -> TMS e b
--   TMS f <*> TMS a = TMS (M.fromList results)
--     where
--       results :: [(S.Set (Supported e), Merged b)]
--       results = getCompose . getCompose $ (Compose . Compose) (M.toList f) <*> (Compose . Compose) (M.toList a)

-- instance (Ord e, Eq a, Mergeable a) => Mergeable (TMS e a) where
--   merge old new =
--       let merged = old <> new
--        in if merged == old then NoChange merged
--                            else Changed merged

-- -- A Monad over evidence; not a proper monad because of Lattice requirement on contents
-- -- instance (Ord e) => Monad (TMS e) where
-- (>>~) :: (Ord e, Mergeable b) => TMS e a -> (a -> TMS e b) -> TMS e b
-- ev >>~ f = eJoin $ fmap f ev

-- eJoin :: (Ord e, Mergeable a) => TMS e (TMS e a) -> TMS e a
-- eJoin (TMS m) = foldMap flatten . M.toList $ m
--   where
--     flatten :: Ord e => (S.Set (Supported e), Merged (TMS e b)) -> TMS e b
--     flatten (e, Contradiction) = TMS $ M.singleton e Contradiction
--     flatten (e, Changed (TMS n)) = TMS $ M.mapKeys (e <>) n
--     flatten (e, NoChange (TMS n)) = TMS $ M.mapKeys (e <>) n

-- implies :: e -> v -> TMS e v
-- implies e v = TMS (M.singleton (S.singleton (pure e)) (pure v))

-- implies' :: S.Set (Supported e) -> v -> TMS e v
-- implies' e v = TMS (M.singleton e (pure v))




-- clean :: (Ord e, Eq b, Lattice b) => TMS e b -> TMS e b
-- clean (TMS m) = TMS . M.fromList $ do
--         (k, v) <- (M.toList m)
--         (k', v') <- (M.toList m)
--         if | k == k' -> return (k, v)
--            | joinLeq v v' -> return (k', v')
--            | joinLeq v' v -> return (k, v)
--            | otherwise -> return $ (k, v) \/ (k', v')

-- instance (Ord e, Monoid e, Num n) => Num (TMS e n) where
--   a + b = liftA2 (+) a b
--   a - b = liftA2 (-) a b
--   a * b = liftA2 (*) a b
--   abs = fmap abs
--   signum = fmap signum
--   fromInteger = pure . fromInteger

-- instance (Ord e, Monoid e, Fractional n) => Fractional (TMS e n) where
--   fromRational = pure . fromRational
--   recip = fmap recip

-- partialCompare :: (PartialOrd a) => a -> a -> Ordering
-- partialCompare a b
--   | not (comparable a  b) = EQ
--   | leq a b = LT
--   | otherwise = GT

-- showBestEvidence :: forall e v. (Show e, Show v, Eq v, Lattice v) => TMS e v -> String
-- showBestEvidence (TMS m) = uncurry showEvidenceLine . foldl1' findBest $ M.toList m
--   where
--     findBest :: (S.Set e, v) -> (S.Set e, v) -> (S.Set e, v)
--     findBest (e, v) (e', v')
--       | joinLeq v v' = (e', v')
--       | joinLeq v' v = (e, v)
--       | otherwise = (e, v)

-- showAllEvidence :: forall e v. (Show e, Show v) => TMS e v -> String
-- showAllEvidence (TMS m) = M.foldMapWithKey showEvidenceLine m

-- showEvidenceLine :: (Show e, Show v) => S.Set e -> v -> String
-- showEvidenceLine es v = (intercalate ", " . fmap show $ (S.toList es)) <> ":\n -> " <> show v <> "\n"

-- evidenceWithout :: (Ord e) => e -> TMS e v -> TMS e v
-- evidenceWithout e (TMS m) = TMS (flip M.filterWithKey m $ \es _ -> not $ e `S.member` es)

-- one, two, three :: Supported String (Interval Int)
-- one = "one" `supports` (I 1 10)
-- two = "two" `supports` (I 3 12)
-- three = "three" `supports` (I 4 6)
-- -- four = "one" `supports` (I 5 6)
-- test :: TMS String (Interval Int)
-- test = foldMap (TMS . S.singleton)
--   [ one
--   , two
--   , three
--   , (Supported (S.fromList ["one", "two"]) (I 5 6))
--   , (Supported (S.fromList ["one"]) (I 4 6))
--   ]
-- -- expected:
-- -- >>> test
-- -- (fromList ["four","three"],Range {rangeMin = 4, rangeMax = 8})
