{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Lib where

import Prelude hiding (Either(..))
import qualified Prelude as Pre

import Control.Monad.Operational
import Control.Monad.State
import Data.Foldable as F
import Data.List.NonEmpty hiding (toList, map, reverse)
import Data.Semigroup
import Data.Text (Text)
import Formatting hiding (left, right)
import System.Console.Haskeline hiding (replacement)
-- import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Debug.Trace
-- trace = flip const
-- traceShow = flip const

-- | Given a list of items, use merge sort where the sort function is YOU
-- :D
uSort :: [Text] -> IO [Text]
uSort [] = pure []
uSort (x:xs) = toList <$> sortFunc userCompare (x :| xs)

class ToNonEmpty m where
    toList' :: m a -> NonEmpty a

instance ToNonEmpty NonEmpty where
    toList' = id

-- | Does a weird even/odd thing, but whatevs
half :: a -> a -> [a] -> (NonEmpty a, NonEmpty a)
half one two rest =
    let halfLength = ((F.length rest + 2) `div` 2) - 2
        (left, right) = Pre.splitAt halfLength rest
    in case (left, right) of
        ([], []) -> (one :| [], two :| [])
        (xs, []) -> (one :| [], two :| xs)
        (xs, (y:ys)) -> (one :| two : xs, y :| ys)

data Provenance
        = L Provenance
        | R Provenance
        | B
        deriving (Show)

data Sorted a = S { val :: a, prov :: Provenance }
        deriving (Show, Functor)

fromLeft, fromRight :: Sorted a -> Sorted a
fromLeft (S a p) = S a (L p)
fromRight (S a p) = S a (R p)

data SortState a
        = Unsorted (NonEmpty a)
        | SortingLeft (SortState a) (NonEmpty a)
        | SortingRight [Sorted a] (SortState a)
        | Merging [Sorted a] [Sorted a] [Sorted a]
        | Sorted [Sorted a]
        deriving (Show)

sortFunc :: User IO [Text] -> NonEmpty Text -> IO [Text]
sortFunc user xs = do
    f <- forward user (Unsorted xs)
    case f of
        Sorted ys -> return (map val ys)
        _ -> sortFunc user xs

forward :: User IO [Text] -> SortState Text -> IO (SortState Text)
forward user zzz = trace ("Fwd | " ++ show zzz) $ case zzz of
    Sorted _ -> pure zzz
    Unsorted (x :| xs) -> case xs of
        [] -> pure (Sorted [S x B])
        (x':xs') ->
            let (left, right) = half x x' xs'
            in fwd (SortingLeft (Unsorted left) right)

    SortingLeft (Sorted ls) rs -> fwd (SortingRight ls (Unsorted rs))
    SortingLeft l rs -> trace "ASCEND" fwd =<< SortingLeft <$> fwd' l <*> pure rs

    SortingRight ls (Sorted rs) -> fwd (Merging ls rs [])
    SortingRight ls r -> trace "ASCEND" fwd =<< SortingRight <$> pure ls <*> fwd' r

    Merging [] rs xs -> pure (Sorted (xs ++ map fromRight rs))
    Merging ls [] xs -> pure (Sorted (xs ++ map fromLeft ls))
    Merging (l:ls) (r:rs) xs -> eval =<< viewT user
      where
        eval = \case
            Return _ -> error "Inconceivable"
            GetNextStep :>>= k -> forward (k (66, 88, val l, val r)) zzz
            Rewrite1 l' :>>= k -> forward (k ()) (Merging ((l' <$ l) : ls) (r:rs) xs)
            Rewrite2 r' :>>= k -> forward (k ()) (Merging (l:ls) ((r' <$ r) : rs) xs)
            Compare o :>>= k -> case (o, ls, rs) of
                (LT, [], _) -> forward (k ()) (Sorted xs')
                  where xs' = xs ++ [fromLeft l] ++ (map fromRight (r:rs))
                (LT, _, _) -> forward (k ()) (Merging ls (r:rs) xs')
                  where xs' = xs ++ [fromLeft l]
                (_ , _, []) -> forward (k ()) (Sorted xs')
                  where xs' = xs ++ [fromRight r] ++ (map fromLeft (l:ls))
                (_ , _, _) -> forward (k ()) (Merging (l:ls) rs xs')
                  where xs' = xs ++ [fromRight r]
            Undo :>>= k -> forward (k ()) =<< bwd zzz
  where
    fwd' = trace "DESCEND" fwd
    fwd = forward user
    bwd = backward user

backward :: User IO [Text] -> SortState Text -> IO (SortState Text)
backward user zzz = trace ("Bwd | " ++ show zzz) $ case zzz of
    Unsorted _ -> error "backward Unsorted"

    SortingLeft (Unsorted ls) rs -> fwd (Unsorted (ls <> rs))
    SortingLeft l rs -> trace "ASCEND" fwd =<< SortingLeft <$> (bwd' l) <*> pure rs

    SortingRight ls (Unsorted rs) -> bwd (SortingLeft (Sorted ls) rs)
    SortingRight ls r -> trace "ASCEND" fwd =<< SortingRight <$> pure ls <*> bwd' r

    Merging l r [] -> bwd (SortingRight l (Sorted r))
    Merging l r (x:xs) -> case (unmerge l r x) of
        (l', []) -> bwd (Merging l' r xs)
        ([], r') -> bwd (Merging l r' xs)
        (l', r') -> fwd (Merging l' r' xs)

  where
    fwd = forward user
    bwd' = trace "DESCEND" bwd
    bwd = backward user

unmerge l r (S a (L p)) = (S a p : l, r)
unmerge l r (S a (R p)) = (l, S a p : r)
unmerge l r (S a B) = error "no wat"

-- ##
-- ## operations we permit the user
-- ##

data UserI a where
    GetNextStep :: UserI (Int, Int, Text, Text)
    Compare     :: Ordering -> UserI ()
    Rewrite1    :: Text -> UserI ()
    Rewrite2    :: Text -> UserI ()
    Undo        :: UserI ()

type User m a = ProgramT UserI m a

getNextStep = singleton GetNextStep

-- Here's a "program".
userCompare :: MonadIO m => User m [Text]
userCompare = forever (getNextStep >>= doSomething)
  where
    doSomething :: MonadIO m => (Int, Int, Text, Text) -> User m ()
    doSomething v@(remain, est, x, y) = do
        liftIO (printPrompt (remain, est, x, y))
        c <- liftIO getResponse
        case c of
            '1' -> singleton (Compare LT)
            '2' -> singleton (Compare GT)
            'e' -> singleton . either Rewrite1 Rewrite2 =<< liftIO (editItem x y)
            'u' -> singleton Undo
            _   -> unknownCommand (doSomething v)

getResponse = getChar <* putStrLn ""

unknownCommand cont = do
    liftIO $ putStrLn "Unknown command. Let's try again."
    cont

editItem :: Text -> Text -> IO (Pre.Either Text Text)
editItem x y = do
    putStr "Which item? > "
    c <- getResponse
    case c of
        '1' -> Pre.Left  <$> replaceText x
        '2' -> Pre.Right <$> replaceText y
        _   -> unknownCommand (editItem x y)

replaceText t = do
    replacement <-
        runInputT defaultSettings
                  (getInputLineWithInitial "What instead? > " ("", T.unpack t))
    return (maybe t T.pack replacement)

printPrompt :: (Int, Int, Text, Text) -> IO ()
printPrompt (remaining, estimate, x, y) = do
    T.putStrLn (sformat hdrFmt remaining estimate)
    T.putStrLn ("-- (1) " <> x)
    T.putStrLn ("-- (2) " <> y)
    T.putStrLn "-- Or [e]dit an entry"
    T.putStrLn "-- Or [u]ndo last comparison"
    T.putStr "-> "
  where
    hdrFmt = "(~" % int % "/" % int % ") Which is more important?"

