{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Lib where

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
    runReaderT (sortFunc xs) mv

-- sortFunc :: ( tOuter ~ ExceptT (MergeFail a) mOuter
--             , tInner ~ ExceptT (MergeFail a) IO
--             , Show a
--             , MonadIO mOuter
--             , MonadReader (MVar (Maybe (CmpT a tInner b))) mOuter)
--          => [a]
--          -> tOuter [Sorted a]
sortFunc [] = pure (Right [])
sortFunc [x] = pure (Right [S x B])
sortFunc xs = do
    el <- sortFunc h1
    case el of
        Left (SortEnded ls) -> traceShow ls $ error "l SortEnded"
        Left (Unsorted ls) -> pure (Left (Unsorted (ls ++ h2)))
        Right l -> goRight l h2
  where
    (h1, h2) = splitAt half xs
    half = length xs `div` 2

goRight l h2 = do
            er <- sortFunc h2
            case er of
                Left (SortEnded rs) -> pure (Left (SortEnded (l ++ rs)))
                Left (Unsorted rs) -> do
                    -- Left is sorted, right failed to sort.
                    el' <- resort l
                    case el' of
                        Left (SortEnded ls') -> traceShow ls' $ error "l resort SortEnded"
                        Left (Unsorted ls') -> pure (Left (Unsorted (ls' ++ rs)))
                        Right l' -> goRight l' rs
                Right r -> goMerge l r

goMerge l r = do
                    em <- runExceptT (merge l r)
                    case em of
                        Left (MergeEnded xs) -> pure (Left (SortEnded xs))
                        Left (Unmerged l' r') -> do
                            er' <- resort r'
                            case er' of
                                Left (SortEnded rs') -> error "r' resort SortEnded"
                                Left (Unsorted rs') -> do
                                    el'' <- resort l'
                                    case el'' of
                                        Left (SortEnded ls'') -> error "l'' resort ended"
                                        Left (Unsorted ls'') -> pure (Left (Unsorted (ls'' ++ rs')))
                                        Right l'' -> goRight l'' rs'
                                Right r'' -> goMerge l' r''
                        Right sorted -> pure (Right sorted)

resort sorted = traceShow sorted $ error "resort"
