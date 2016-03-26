{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Lib2 where

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Data.List
import Data.Foldable
import Control.Monad.State
import Control.Error

import Sorted
import Merge

import Debug.Trace

data SortFail a = SortEnded [Sorted a]
                | Unsorted [a]
    deriving (Show)

usort p xs = do
    mv <- newMVar (Just p)
    runReaderT (runExceptT (sortFunc xs)) mv

-- sortFunc :: ( tOuter ~ ExceptT (MergeFail a) mOuter
--             , tInner ~ ExceptT (MergeFail a) IO
--             , Show a
--             , MonadIO mOuter
--             , MonadReader (MVar (Maybe (CmpT a tInner b))) mOuter)
--          => [a]
--          -> tOuter [Sorted a]
sortFunc [] = pure []
sortFunc [x] = pure [S x B]
sortFunc xs = do
    l <- sortFunc h1 -- fail immediately if left sort fails
    goRight l h2
  where
    (h1, h2) = splitAt half xs
    half = length xs `div` 2

goRight l h2 = do
    r <- sortFunc h2 `catchE` retryLeft l
    goMerge l r

-- goMerge :: ( tOuter ~ ExceptT (SortFail a) mOuter
--            , tInner ~ ExceptT (MergeFail a) IO
--            , Show a
--            , MonadIO mOuter
--            , MonadReader (MVar (Maybe (CmpT a tInner b))) mOuter)
--         => [Sorted a]
--         -> [Sorted a]
--         -> tOuter [Sorted a]
goMerge l r = merge l r `catchE` retryRight
  where
    retryRight (MergeEnded xs) = throwE (SortEnded xs)
    retryRight (Unmerged failLeft failRight) = do
        r' <- resort failRight `catchE` retryLeft failLeft
        goMerge failLeft r'

retryLeft l (SortEnded xs) = throwE (SortEnded (l ++ xs))
retryLeft l (Unsorted right) = do
    l' <- withExceptT (appendRight right) (resort l)
    goRight l' right
  where
    appendRight _ (SortEnded _) = error "appendRight SortEnded"
    appendRight rs (Unsorted xs) = Unsorted (xs ++ rs)

resort [x] = throwE (Unsorted [(val x)])
resort xs = traceShow xs $ error "resort"
