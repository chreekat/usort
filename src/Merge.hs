{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Merge (merge, MergeFail(..), CmpT(..), CmpI(..)) where

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
         , tOuter ~ ExceptT (MergeFail a) mOuter
         , tInner ~ ExceptT (MergeFail a) IO
         , MonadReader (MVar (Maybe (CmpT a tInner b))) mOuter
         , MonadIO mOuter)
      => [Sorted a]
      -> [Sorted a]
      -> tOuter [Sorted a]
merge l r = do
    mvar <- lift ask
    mprog <- liftIO (takeMVar mvar)
    case mprog of
        Just prog -> ExceptT (liftIO (runExceptT (go prog mvar l r [])))
        Nothing -> do
            liftIO (putMVar mvar Nothing)
            throwE (MergeEnded (l ++ r))

go :: ( Show a
      , t ~ ExceptT (MergeFail a) IO)
   => CmpT a t b
   -> MVar (Maybe (CmpT a t b))
   -> [Sorted a]
   -> [Sorted a]
   -> [Sorted a]
   -> t [Sorted a]
go p mvar left right result = traceShow (left,right) $ case (left, right) of
    ([],[]) -> do
        putProg p
        pure result
    (ls,[]) -> do
        putProg p
        pure (result ++ map fromLeft ls)
    ([],rs) -> do
        putProg p
        pure (result ++ map fromRiht rs)
    (l:ls,r:rs) -> eval (l :| ls) (r :| rs) =<< viewT p
  where
    eval (l :| ls) (r :| rs) = \case
        Return _ -> do
            putNoProg
            throwE (MergeEnded (result ++ (l:ls) ++ (r:rs)))
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
            (S x (L p)) : ress ->
                go (k ()) mvar (S x p : l : ls) (r : rs) ress
            (S x (R p)) : ress ->
                go (k ()) mvar (r : rs) (S x p : r : rs) ress
            _ -> do
                putProg (k ())
                throwE (Unmerged (l : ls) (r : rs))
    putProg p = liftIO $ putMVar mvar (Just p)
    putNoProg = liftIO $ putMVar mvar Nothing

type CmpT v m a = ProgramT (CmpI v) m a

data CmpI v a where
    GetNextStep :: CmpI v (Int, Int, v, v)
    Compare     :: Ordering -> CmpI v ()
    Rewrite1    :: v -> CmpI v ()
    Rewrite2    :: v -> CmpI v ()
    Undo        :: CmpI v ()

data MergeFail a = MergeEnded [Sorted a]
                 | Unmerged [Sorted a] [Sorted a]
                 deriving (Show)
