{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Lib2 where

import Control.Monad.Operational
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.List
import Data.Foldable
import Control.Monad.State
import Control.Error
import qualified Data.List.NonEmpty as NE

import Debug.Trace

-- hellaSort prog xs =
--     evalStateT (runExceptT (sortList xs)) prog

-- sortList xs = sortFunc xs `catchE` (\again -> sortList again)

sortFunc [] _ = pure []
sortFunc [x] _ = pure [S x B]
sortFunc xs p = do
    l <- sortFunc h1 p -- fail immediately if left sort fails
    sortFail $ do
        (l', r) <- goRight l h2 p
        goMerge l' r p
  where
    (h1, h2) = splitAt half xs
    half = length xs `div` 2
    sortFail = withExceptT (\(l,r) -> l ++ r)

goRight l h2 p = soSimple `catchE` retryLeft
  where
    soSimple = (,) <$> pure l <*> sortFunc h2 p
    retryLeft h2' = join (goRight <$> resort l p <*> pure h2' <*> pure p)

goMerge l r p = soSimple `catchE` retryRight `catchE` retryLeft
  where
    soSimple = merge l r p
    retryRight (failedLeft, failedRight) =
        join (goMerge <$> pure failedLeft <*> resort failedRight <*> pure p)
    retryLeft (failedLeft, failedRight) = do
        resortedLeft <- resort failedLeft p
        (newLeft, newRight) <- goRight resortedLeft failedRight p
        goMerge newLeft newRight p

unmerge = undefined
merge l r p = infinityAndBeyond p l r []

infinityAndBeyond :: (m ~ ExceptT ([Sorted a], [Sorted a]) m2
                     , Monad m2
                     , Show b
                     , Show a)
                  => ProgramT (SortI a) m b
                  -> [Sorted a]
                  -> [Sorted a]
                  -> [Sorted a]
                  -> m [Sorted a]
infinityAndBeyond p left right result = traceShow (left,right) $ case (left, right) of
    ([],[]) -> pure result
    (ls,[]) -> pure (result ++ map fromLeft ls)
    ([],rs) -> pure (result ++ map fromRight rs)
    (l:ls,r:rs) ->
        eval (NE.reverse (l :| ls)) (NE.reverse (r :| rs)) result =<< viewT p
  where
    eval (l :| ls) (r :| rs) result = \case
        Return x -> pure $ l : (ls ++ [r] ++ rs++result)
        GetNextStep :>>= k ->
            infinityAndBeyond (k (66, 88, val l, val r)) (l : ls) (r : rs) result
        Rewrite1 newLeft :>>= k ->
            infinityAndBeyond (k ()) ((newLeft <$ l) : ls) (r : rs) result
        Rewrite2 newRight :>>= k ->
            infinityAndBeyond (k ()) (l : ls) ((newRight <$ r) : rs) result
        Compare o :>>= k -> case o of
            LT -> infinityAndBeyond (k ()) ls (r : rs) (result ++ [fromLeft l])
            _  -> infinityAndBeyond (k ()) (l : ls) rs (result ++ [fromRight r])
        Undo :>>= k -> case result of
            (S x (L p)) : ress ->
                infinityAndBeyond (k ()) (S x p : l : ls) (r : rs) ress
            (S x (R p)) : ress ->
                infinityAndBeyond (k ()) (r : rs) (S x p : r : rs) ress
            _ -> throwE (r : rs, l : ls)

resort = undefined

fromLeft (S x p) = S x (L p)
fromRight (S x p) = S x (R p)


data SortI v a where
    GetNextStep :: SortI v (Int, Int, v, v)
    Compare     :: Ordering -> SortI v ()
    Rewrite1    :: v -> SortI v ()
    Rewrite2    :: v -> SortI v ()
    Undo        :: SortI v ()

data Provenance
        = L Provenance
        | R Provenance
        | B

instance Show Provenance where
    show B = "B"
    show (L p) = "L:" ++ show p
    show (R p) = "R:" ++ show p


data Sorted a = S a Provenance
    deriving (Functor)

instance Show a => Show (Sorted a) where
    show (S a p) = show a ++ ":" ++ show p

val (S a _) = a
