{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
module Sort where

import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent

import Sorted
import Merge

data SortFail a = SortEnded [Sorted a]
                | Unsorted [a]
    deriving (Show, Eq)

sortFunc
  :: ( Show a
     , MonadReader (MVar (Maybe (MrgT a (StateT (Int,Int) IO) b))) f
     , MonadState (Int, Int) f, MonadIO f)
  => [a]
  -> f (Either (SortFail a) [Sorted a])
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
    bases = map (`S` B)

redoLeft
  :: (Show a, MonadReader (MVar (Maybe (MrgT a (StateT (Int,Int) IO) b))) f,
      MonadState (Int, Int) f, MonadIO f) =>
     [Sorted a] -> [a] -> f (Either (SortFail a) [Sorted a])
redoLeft l rs = do
    el' <- resort l
    case el' of
        Left (SortEnded ls') -> pure (Left (SortEnded (ls' ++ map base rs)))
        Left (Unsorted ls') -> pure (Left (Unsorted (ls' ++ rs)))
        Right l' -> goRight l' rs

goRight
  :: (Show a, MonadReader (MVar (Maybe (MrgT a (StateT (Int,Int) IO) b))) f,
      MonadState (Int, Int) f, MonadIO f) =>
     [Sorted a] -> [a] -> f (Either (SortFail a) [Sorted a])
goRight l h2 = do
    er <- sortFunc h2
    case er of
        Left (SortEnded rs) -> pure (Left (SortEnded (l ++ rs)))
        Left (Unsorted rs) -> redoLeft l rs
        Right r -> goMerge l r []

redoRight
  :: (Show a, MonadReader (MVar (Maybe (MrgT a (StateT (Int,Int) IO) b))) f,
      MonadState (Int, Int) f, MonadIO f) =>
     [Sorted a] -> [Sorted a] -> f (Either (SortFail a) [Sorted a])
redoRight l' r' = do
    er' <- resort r'
    case er' of
        Left (SortEnded rs') -> pure (Left (SortEnded (l' ++ rs')))
        Left (Unsorted rs') -> redoLeft l' rs'
        Right r'' -> goMerge l' r'' []

resort
  :: (Show a, MonadReader (MVar (Maybe (MrgT a (StateT (Int,Int) IO) b))) f,
      MonadState (Int, Int) f, MonadIO f) =>
     [Sorted a] -> f (Either (SortFail a) [Sorted a])
resort = go [] []
  where
    go (l:ls) (r:rs) xs     = goMerge (l:ls) (r:rs) xs
    go left   right  []     = goMerge left right []
    go left   right  (x:xs) =
        let (xs', [S final p]) = splitAt (length xs) (x:xs)
        in case p of
            L p' -> go (S final p' : left) right xs'
            R p' -> go left (S final p' : right) xs'
            B -> pure (Left (Unsorted (map val (left ++ right ++ (x:xs)))))

goMerge
  :: (Show a, MonadReader (MVar (Maybe (MrgT a (StateT (Int,Int) IO) b))) f,
      MonadState (Int, Int) f, MonadIO f) =>
     [Sorted a] -> [Sorted a] -> [Sorted a] -> f (Either (SortFail a) [Sorted a])
goMerge l r initial = do
    em <- merge l r initial
    case em of
        Left (MergeEnded xs) -> pure (Left (SortEnded xs))
        Left (Unmerged l' r') -> redoRight l' r'
        Right sorted -> pure (Right sorted)

retrySort :: Show v => MrgT v (StateT (Int, Int) IO) a -> [v] -> IO (Either [v] [v])
retrySort fn input = runReaderT (flip evalStateT (n', n') (go input)) =<< newMVar (Just fn)
  where
    go xs = do
        res <- sortFunc xs
        case res of
            Left (Unsorted xs') -> go xs'
            Left (SortEnded xs') -> pure (Left (map val xs'))
            Right xs' -> pure (Right (map val xs'))
    n' = nlogn (length input)

nlogn :: Int -> Int
nlogn n = let g = fromRational . fromIntegral $ n
          in floor (g * log g :: Double)
