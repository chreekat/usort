{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Lib
    ( sortFunc
    ) where

import Data.Text (Text)

type MonadCompare m a = a -> a -> m Ordering

-- | Given a list of items, use merge sort where the sort function is YOU
-- :D
sortFunc :: [Text] -> IO [Text]
sortFunc = undefined

-- An undoable, monadic merge.
umadSort :: Monad m => MonadCompare m a -> [a] -> m [a]
umadSort = undefined

-- To start, though, let's not do undos.
uSort :: Monad m => MonadCompare m a -> [a] -> m [a]
uSort f = \case
    []  -> pure []
    [x] -> pure [x]
    xs  -> do
        -- Walking the list a bunch of times here, but compared to waiting
        -- for the user to do each comparison, ... it's pretty
        -- insignificant.
        e <- uSort f (evens xs)
        o <- uSort f (odds xs)
        merge f e o

merge :: Monad m => MonadCompare m a -> [a] -> [a] -> m [a]
merge f xs' ys' = case (xs', ys') of
    ([], []) -> pure []
    ([], ys) -> pure ys
    (xs, []) -> pure xs
    (x:xs, y:ys) -> do
        o <- f x y
        case o of
            LT -> (:) <$> pure x <*> merge f xs ys'
            _  -> (:) <$> pure y <*> merge f xs' ys

evens, odds :: [a] -> [a]
evens = \case
    []     -> []
    (x:xs) -> odds xs

odds = \case
    []     -> []
    [x]    -> [x]
    (x:xs) -> x : evens xs
