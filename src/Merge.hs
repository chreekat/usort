{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Merge (merge, MergeFail(..), MrgT, MrgI(..)) where

import Control.Monad.Operational
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.MVar
import Data.List.NonEmpty (NonEmpty(..))
import Lens.Family.State
import Lens.Family.Stock

import Sorted

merge :: ( Show a
         , MonadReader (MVar (Maybe (MrgT a (StateT (Int, Int) IO) b))) mOuter
         , MonadState (Int, Int) mOuter
         , MonadIO mOuter)
      => [Sorted a]
      -> [Sorted a]
      -> [Sorted a]
      -> mOuter (Either (MergeFail a) [Sorted a])
merge l r initial = do
    mvar <- ask
    ppos <- get
    mprog <- liftIO (takeMVar mvar)
    case mprog of
        Just prog -> do
            (rslt, ploop) <- liftIO (flip runStateT ppos (go prog mvar l r initial))
            put ploop
            pure rslt
        Nothing -> do
            liftIO (putMVar mvar Nothing)
            pure (Left (MergeEnded (l ++ r)))

-- | We must use MVar, and not e.g. State, because the MrgT value needs to
-- be in the "left side" (positive? Negative? I forget the term) to avoid a
-- infinitely-recursing type. Similarly, we must concretely use IO.
go :: ( Show a )
   => MrgT a (StateT (Int, Int) IO) b
   -> MVar (Maybe (MrgT a (StateT (Int, Int) IO) b))
   -> [Sorted a]
   -> [Sorted a]
   -> [Sorted a]
   -> StateT (Int, Int) IO (Either (MergeFail a) [Sorted a])
go prog mvar left right result = {-traceShow (left,right,result) $-} case (left, right) of
    ([],[]) -> do
        putProg prog
        pure (Right result)
    (ls,[]) -> do
        putProg prog
        pure (Right (result ++ map fromLeft ls))
    ([],rs) -> do
        putProg prog
        pure (Right (result ++ map fromRiht rs))
    (l:ls,r:rs) -> eval (l :| ls) (r :| rs) =<< viewT prog
  where
    eval (l :| ls) (r :| rs) = \case
        Return _ -> do
            putNoProg
            pure (Left (MergeEnded (result ++ (l:ls) ++ (r:rs))))
        GetNextStep :>>= k -> get >>= \(tot, rmn) ->
            go (k (rmn, tot, val l, val r)) mvar (l : ls) (r : rs) result
        Rewrite LT newLeft :>>= k ->
            go (k ()) mvar ((newLeft <$ l) : ls) (r : rs) result
        Rewrite _  newRiht :>>= k ->
            go (k ()) mvar (l : ls) ((newRiht <$ r) : rs) result
        Compare o :>>= k -> _2 %= pred >> case o of
            LT -> go (k ()) mvar ls (r : rs) (result ++ [fromLeft l])
            _  -> go (k ()) mvar (l : ls) rs (result ++ [fromRiht r])
        Undo :>>= k -> _2 %= succ' >> case result of
            S x (L p) : ress ->
                go (k ()) mvar (S x p : l : ls) (r : rs) ress
            S x (R p) : ress ->
                go (k ()) mvar (l : ls) (S x p : r : rs) ress
            _ -> do
                putProg (k ())
                pure (Left (Unmerged (l : ls) (r : rs)))
        Delete LT :>>= k ->
            go (k ()) mvar ls (r : rs) result
        Delete _  :>>= k ->
            go (k ()) mvar (l : ls) rs result
    putProg p = liftIO $ putMVar mvar (Just p)
    putNoProg = liftIO $ putMVar mvar Nothing
    succ' n | n > 0 = succ n
            | otherwise = n

type MrgT v m a = ProgramT (MrgI v) m a

data MrgI v a where
    GetNextStep :: MrgI v (Int, Int, v, v)
    Compare     :: Ordering -> MrgI v ()
    Rewrite     :: Ordering -> v -> MrgI v ()
    Undo        :: MrgI v ()
    Delete      :: Ordering -> MrgI v ()

data MergeFail a = MergeEnded [Sorted a]
                 | Unmerged [Sorted a] [Sorted a]
                 deriving (Show)
