-- | Remember comparisons.
module Remember where

import Control.Applicative
import Data.Coerce
import Data.Hashable
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

-- | Keep a list of seen comparisons
newtype Compared = Compared (Map.Map (Int, Int) Ordering)
    deriving (Eq, Show)

uncompared = Compared (Map.empty)

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

-- | Divine whether two items have ever been compared
divine :: Int -> Int -> Compared -> Maybe Ordering
divine i j (Compared c) = Map.lookup (i, j) c <|> fmap inv (Map.lookup (j, i) c)
  where
    inv LT = GT
    inv GT = LT
    inv EQ = EQ

-- | Store an ordering if it's new
sto :: Int -> Int -> Ordering -> Compared -> Compared
sto i j which c =
    case divine i j c of
        Just _ -> c
        Nothing -> coerce (Map.insert (i, j) which) c

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
