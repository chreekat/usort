{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Remember comparisons.
module Remember where

import Control.Applicative
import Data.Coerce
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

-- | A class of invertible types.
--
-- A law would be invert (invert a) == a.
class Invertible a where
    invert :: a -> a

-- | Keep a list of seen comparisons. It maps two values to a comparison result.
newtype Compared val cmp = Compared (Map.Map (val, val) cmp)
    deriving (Eq, Show, Semigroup, Monoid)

-- | Check if two values have a comparison
recompare :: (Ord val, Invertible cmp) => val -> val -> Compared val cmp -> Maybe cmp
recompare a b (Compared c) =
    (Map.lookup (a, b) c) <|> (invert <$> Map.lookup (b, a) c)

-- | Observe a comparison
observe :: (Ord val, Invertible cmp) => val -> val -> cmp -> Compared val cmp -> Compared val cmp
observe a b cmp c =
    case recompare a b c of
        Just _ -> c
        Nothing -> coerce (Map.insert (a, b) cmp) c

-- | Things remembered.
newtype Mem'd a = Mem'd (IntMap.IntMap a) deriving (Eq, Show)

-- | Given a list of things, keep them and their keys, forever.
remember :: (Foldable f) => f a -> Mem'd a
remember = Mem'd . IntMap.fromAscList . zip [1..] . toList

-- | Remember ints
rememberInts :: (Foldable f) => f Int -> Mem'd Int
rememberInts = Mem'd . IntMap.fromList . fmap (\x -> (x,x)) . toList

memKeys (Mem'd m) = IntMap.keys m
recall i (Mem'd m) = m IntMap.! i

noMem'd = Mem'd (IntMap.empty)

stoMem'd :: Int -> a -> Mem'd a -> Mem'd a
stoMem'd i a = coerce (IntMap.insert i a)

{- | Properties
 -
 - * sto is idempotent
 -
 - forall i j o@(elem [LT GT]) c -> let new = sto i j o c
                                    in divine i j new = Just o
 -                                     divine j i new = Just (inv o)
 -
 - * order matters
 -
 - forall i j o@(elem [LT GT]) -> divine j i (sto i j o Ø) == Just (inv o)
 -}
