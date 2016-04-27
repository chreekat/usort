{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Merge (merge, MergeFail(..), MrgT(..), MrgI(..)) where

import Control.Error
import Control.Monad.Operational
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Debug.Trace

import Sorted

merge :: ( Show a
         , MonadReader (MVar (Maybe (MrgT a IO b))) mOuter
         , MonadIO mOuter)
      => [Sorted a]
      -> [Sorted a]
      -> [Sorted a]
      -> mOuter (Either (MergeFail a) [Sorted a])
merge l r init = do
    mvar <- ask
    mprog <- liftIO (takeMVar mvar)
    case mprog of
        Just prog -> liftIO (go prog mvar l r init)
        Nothing -> do
            liftIO (putMVar mvar Nothing)
            pure (Left (MergeEnded (l ++ r)))

-- | We must use MVar, and not e.g. State, because the MrgT value needs to
-- be in the "left side" (positive? Negative? I forget the term) to avoid a
-- infinitely-recursing type. Similarly, we must concretely use IO.
go :: ( Show a )
   => MrgT a IO b
   -> MVar (Maybe (MrgT a IO b))
   -> [Sorted a]
   -> [Sorted a]
   -> [Sorted a]
   -> IO (Either (MergeFail a) [Sorted a])
go p mvar left right result = {-traceShow (left,right,result) $-} case (left, right) of
    ([],[]) -> do
        putProg p
        pure (Right result)
    (ls,[]) -> do
        putProg p
        pure (Right (result ++ map fromLeft ls))
    ([],rs) -> do
        putProg p
        pure (Right (result ++ map fromRiht rs))
    (l:ls,r:rs) -> eval (l :| ls) (r :| rs) =<< viewT p
  where
    eval (l :| ls) (r :| rs) = \case
        Return _ -> do
            putNoProg
            pure (Left (MergeEnded (result ++ (l:ls) ++ (r:rs))))
        GetNextStep :>>= k ->
            go (k (66, 88, val l, val r)) mvar (l : ls) (r : rs) result
        Rewrite1 newLeft :>>= k ->
            go (k ()) mvar ((newLeft <$ l) : ls) (r : rs) result
        Rewrite2 newRiht :>>= k ->
            go (k ()) mvar (l : ls) ((newRiht <$ r) : rs) result
        Compare o :>>= k -> case o of
            LT -> go (k ()) mvar ls (r : rs) (result ++ [fromLeft l])
            _  -> go (k ()) mvar (l : ls) rs (result ++ [fromRiht r])
        Undo :>>= k -> case result of
            S x (L p) : ress ->
                go (k ()) mvar (S x p : l : ls) (r : rs) ress
            S x (R p) : ress ->
                go (k ()) mvar (l : ls) (S x p : r : rs) ress
            _ -> do
                putProg (k ())
                pure (Left (Unmerged (l : ls) (r : rs)))
    putProg p = liftIO $ putMVar mvar (Just p)
    putNoProg = liftIO $ putMVar mvar Nothing

type MrgT v m a = ProgramT (MrgI v) m a

data MrgI v a where
    GetNextStep :: MrgI v (Int, Int, v, v)
    Compare     :: Ordering -> MrgI v ()
    Rewrite1    :: v -> MrgI v ()
    Rewrite2    :: v -> MrgI v ()
    Undo        :: MrgI v ()

data MergeFail a = MergeEnded [Sorted a]
                 | Unmerged [Sorted a] [Sorted a]
                 deriving (Show)
