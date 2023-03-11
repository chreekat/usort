{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Remember comparisons.
module Compared where

import Control.Applicative
import Data.Coerce
import Data.Foldable
import qualified Data.Map as Map

-- | A class of invertible types.
--
-- A law would be invert (invert a) == a.
class Invertible a where
    invert :: a -> a

-- | Keep a list of seen comparisons. It maps two values to a comparison result.
newtype Compared val res = Compared (Map.Map (val, val) res)
    deriving (Eq, Show, Semigroup, Monoid)

-- | Check if two values have a comparison
recompare :: (Ord val, Invertible res) => val -> val -> Compared val res -> Maybe res
recompare a b (Compared c) =
    (Map.lookup (a, b) c) <|> (invert <$> Map.lookup (b, a) c)

-- | Observe a comparison
observe :: (Ord val, Invertible cmp) => val -> val -> cmp -> Compared val cmp -> Compared val cmp
observe a b cmp c =
    case recompare a b c of
        Just _ -> c
        Nothing -> coerce (Map.insert (a, b) cmp) c




-- | I want to remember comparisons between elements even if the element is
-- later edited. Thus I need an initial, stable key to track actions and
-- elements separately. If I simply use the initial value as the stable key, it
-- may simplify tests later by allowing me to make the mapping optional.
newtype ElementMap a = ElementMap (Map.Map a a) deriving (Eq, Show, Semigroup, Monoid)

-- | Given a list of things, keep them and their keys, forever.
elementMap :: (Foldable f, Ord a) => f a -> ElementMap a
elementMap = coerce . Map.fromList . fmap (\x -> (x,x)) . toList

element :: Ord a => a -> ElementMap a -> a
element a (ElementMap m) = m Map.! a

insertEl :: Ord a => a -> a -> ElementMap a -> ElementMap a
insertEl k v = coerce (Map.insert k v)
