{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
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
    deriving (Show, Eq)

-- sortFunc :: ( Show a
--          , MonadReader (MVar (Maybe (MrgT a IO b))) mOuter
--          , MonadIO mOuter)
--       => [a]
--       -> mOuter (Either (SortFail a) [Sorted a])
sortFunc
  :: (Show a, MonadReader (MVar (Maybe (MrgT a IO b))) f,
      MonadIO f) =>
     [a] -> f (Either (SortFail a) [Sorted a])
sortFunc [] = pure (Right [])
sortFunc [x] = pure (Right [S x B])
sortFunc xs = do
    el <- sortFunc h1
    case el of
        Left (SortEnded ls) -> pure (Left (SortEnded (ls ++ bases h2)))
        Left (Unsorted ls) -> pure (Left (Unsorted (ls ++ h2)))
        Right l -> goRight l h2
  where
    (h1, h2) = splitAt half xs
    half = length xs `div` 2
    bases = map (\x -> S x B)

redoLeft
  :: (Show a, MonadReader (MVar (Maybe (MrgT a IO b))) f,
      MonadIO f) =>
     [Sorted a] -> [a] -> f (Either (SortFail a) [Sorted a])
redoLeft l rs = do
    el' <- resort l
    case el' of
        Left (SortEnded ls') -> traceShow ls' $ error "l resort SortEnded"
        Left (Unsorted ls') -> pure (Left (Unsorted (ls' ++ rs)))
        Right l' -> goRight l' rs

goRight
  :: (Show a, MonadReader (MVar (Maybe (MrgT a IO b))) f,
      MonadIO f) =>
     [Sorted a] -> [a] -> f (Either (SortFail a) [Sorted a])
goRight l h2 = do
    er <- sortFunc h2
    case er of
        Left (SortEnded rs) -> pure (Left (SortEnded (l ++ rs)))
        Left (Unsorted rs) -> redoLeft l rs
        Right r -> goMerge l r []

redoRight
  :: (Show a, MonadReader (MVar (Maybe (MrgT a IO b))) f,
      MonadIO f) =>
     [Sorted a] -> [Sorted a] -> f (Either (SortFail a) [Sorted a])
redoRight l' r' = do
    er' <- resort r'
    case er' of
        Left (SortEnded rs') -> error "r' resort SortEnded"
        Left (Unsorted rs') -> redoLeft l' rs'
        Right r'' -> goMerge l' r'' []

resort
  :: (Show a, MonadReader (MVar (Maybe (MrgT a IO b))) f,
      MonadIO f) =>
     [Sorted a] -> f (Either (SortFail a) [Sorted a])
resort = go [] []
  where
    go (l:ls) (r:rs) = goMerge (l:ls) (r:rs)
    go left right = \case
        [] -> pure (Right [])
        [x] -> pure (Right [x])
        (x:xs) ->
            let (xs', [S last p]) = splitAt (length xs) (x:xs)
            in case p of
                L p' -> go ((S last p') : left) right xs'
                R p' -> go left ((S last p') : right) xs'
                B -> error "wat"

goMerge
  :: (Show a, MonadReader (MVar (Maybe (MrgT a IO b))) f,
      MonadIO f) =>
     [Sorted a] -> [Sorted a] -> [Sorted a] -> f (Either (SortFail a) [Sorted a])
goMerge l r init = do
    em <- merge l r init
    case em of
        Left (MergeEnded xs) -> pure (Left (SortEnded xs))
        Left (Unmerged l' r') -> redoRight l' r'
        Right sorted -> pure (Right sorted)

