{-# LANGUAGE FlexibleContexts #-}
module Lib2 where

import Control.Monad.Operational
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.List
import Data.Foldable
import Control.Monad.State

tryForever :: MonadPlus m => m a -> m a
tryForever = msum . repeat

hellaSorts xs = tryForever (sortList xs)

ffff = do
    l <- getLine
    case "p" `isPrefixOf` l of
        True -> pure "ha"
        False -> mzero

-- fail immediately if left sort fails.
-- if right side fails, all is not lost. We can still do the left side
-- again. Try resorting it. If resorting left fails, well shit. But if it
-- doesn't, we can do the right side again!

sortList [] = undefined
sortList [x] = undefined
sortList xs = do
    l <- sortList h1 -- fail immediately if left sort fails
    (l', r) <- goRight l h2
    goMerge l' r h2
  where
    (h1, h2) = splitAt half xs
    half = length xs `div` 2

goRight l h2 = soSimple `mplus` retryLeft
  where
    soSimple = (,) <$> pure l <*> sortList h2
    retryLeft = join (goRight <$> resort l <*> pure h2)

goMerge l r h2 = soSimple `mplus` retryRight `mplus` retryLeft
  where
    soSimple = merge l r
    retryRight = join (goMerge <$> pure l <*> resort r <*> pure h2)
    retryLeft = do
        l' <- resort l
        (l'', r') <- goRight l' h2
        goMerge l'' r' h2

-- merge = undefined
merge l r = infinityAndBeyond l r []

infinityAndBeyond left right result = case (toList left, toList right) of
    ([],[]) -> pure result
    (ls,[]) -> pure (result ++ foldMap fromLeft ls)
    ([],rs) -> pure (result ++ foldMap fromRight rs)
    (l:ls,r:rs) -> (eval (l :| ls) (r :| rs) <=< viewT) =<< get


resort = undefined

fromLeft = undefined
fromRight = undefined
eval = undefined
