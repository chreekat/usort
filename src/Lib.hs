{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Lib where

import Prelude as Pre

import Control.Monad.Operational
import Control.Monad.State
import Data.Foldable as F
import Data.List.NonEmpty hiding (toList, map, reverse)
import Data.Semigroup
import Data.Text (Text)
import Formatting hiding (left, right)
import System.Console.Haskeline hiding (replacement)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- import Debug.Trace
trace :: String -> a -> a
trace = flip const
traceShow = flip const

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
        (xs, y:ys) -> (one :| two : xs, y :| ys)

data Provenance
        = L Provenance
        | R Provenance
        | B

data Sorting a
        = Abort
        | Sorting a
        | Unsorting
        deriving (Show, Functor)

instance Applicative Sorting where
    pure = Sorting
    (Sorting f) <*> (Sorting a) = Sorting (f a)
    (Sorting _) <*> Abort = Abort
    (Sorting _) <*> Unsorting = Unsorting
    Abort <*> _ = Abort
    Unsorting <*> _ = Unsorting

type TextSort = Sorting (SortState Text)

instance Show Provenance where
    show B = "B"
    show (L p) = "L:" ++ show p
    show (R p) = "R:" ++ show p

data Sorted a = S a Provenance
        deriving (Functor)

instance Show a => Show (Sorted a) where
    show (S a p) = show a ++ ":" ++ show p

val (S a _) = a

fromLeft, fromRight :: Sorted a -> Sorted a
fromLeft (S a p) = S a (L p)
fromRight (S a p) = S a (R p)

data SortState a
        = Unsorted (NonEmpty a)
        | SortingLeft (SortState a) (NonEmpty a)
        | SortingRight [Sorted a] (SortState a)
        | Merging [Sorted a] [Sorted a] [Sorted a]
        | Sorted [Sorted a]

instance Foldable SortState where
    foldMap f = \case
        Sorted xs -> foldMap f (map val xs)
        Unsorted xs -> foldMap f xs
        SortingLeft l rs -> foldMap f l `mappend` foldMap f rs
        SortingRight ls r -> foldMap f (map val ls) `mappend` foldMap f r
        Merging lll rrr result -> go lll rrr result
            where go ls rs [] = foldMap f (map val ls) `mappend` foldMap f (map val rs)
                  go ls rs (x:xs) = case unmerge ls rs last' of
                      Right (ls', rs') -> go ls' rs' init'
                      Left a -> f a
                    where last' = NE.last unempty
                          init' = NE.init unempty
                          unempty = x :| xs

instance Show a => Show (SortState a) where
    show (Unsorted xs) = "Unsorted " ++ show (toList xs)
    show (SortingLeft l rs) =
        "SortingLeft ╞" ++ show l ++ "╡ " ++ show (toList rs)
    show (SortingRight l r) =
        "SortingRight ╞" ++ show l ++ "╡ ╞" ++ show r ++ "╡"
    show (Merging ls rs results) =
        "Merging " ++ show ls ++ " " ++ show rs ++ " " ++ show results
    show (Sorted ls) = "Sorted " ++ show ls

nlogn :: Int -> Int
nlogn n = let g = fromRational . fromIntegral $ n
          in floor (g * log g :: Double)

sortFunc :: User (StateT (Int,Int) IO) TextSort -> NonEmpty Text -> IO [Text]
sortFunc user xs = go (count,count) (Unsorted xs)
  where
    count = nlogn (NE.length xs)
    go ct step = do
        (f, remaining) <- runStateT (forward user step) ct
        trace "PIP!" $ case f of
            Sorting (Sorted ys) -> return (map val ys)
            Sorting next -> go remaining next
            Unsorting -> go remaining (trace "NO IDEA LOL" (backward step "Bwd | "))
            Abort -> pure (toList step)

forward :: User (StateT (Int,Int) IO) TextSort -> SortState Text -> StateT (Int,Int) IO TextSort
forward user zzz = trace ("Fwd | " ++ show zzz) $ case zzz of
    Sorted _ -> go zzz
    Unsorted (x :| xs) -> case xs of
        [] -> go (Sorted [S x B])
        (x':xs') ->
            let (left, right) = half x x' xs'
            in go (SortingLeft (Unsorted left) right)

    SortingLeft (Sorted ls) rs -> go (SortingRight ls (Unsorted rs))
    SortingLeft l rs -> do
        newL <- fwd' l
        pure (SortingLeft <$> newL <*> pure rs)

    SortingRight ls (Sorted rs) -> go (Merging ls rs [])
    SortingRight ls r -> do
        newR <- fwd' r
        pure (SortingRight <$> pure ls <*> newR)

    Merging [] rs xs -> go (Sorted (xs ++ map fromRight rs))
    Merging ls [] xs -> go (Sorted (xs ++ map fromLeft ls))
    Merging (l:ls) (r:rs) xs -> do
        (ct,orig) <- get
        eval ct orig =<< viewT user
      where
        eval ct orig = \case
            Return _ -> pure Abort
            GetNextStep :>>= k -> forward (k (ct, orig, val l, val r)) zzz
            Rewrite1 l' :>>= k -> forward (k ()) (Merging ((l' <$ l) : ls) (r:rs) xs)
            Rewrite2 r' :>>= k -> forward (k ()) (Merging (l:ls) ((r' <$ r) : rs) xs)
            Compare o :>>= k -> modify pred' >> case (o, ls, rs) of
                (LT, [], _) -> forward (k ()) (Sorted xs')
                  where xs' = xs ++ [fromLeft l] ++ map fromRight (r:rs)
                (LT, _, _) -> forward (k ()) (Merging ls (r:rs) xs')
                  where xs' = xs ++ [fromLeft l]
                (_ , _, []) -> forward (k ()) (Sorted xs')
                  where xs' = xs ++ [fromRight r] ++ map fromLeft (l:ls)
                (_ , _, _) -> forward (k ()) (Merging (l:ls) rs xs')
                  where xs' = xs ++ [fromRight r]
            Undo :>>= k -> trace "UNDONE :(" $ back zzz
              where
                back z = case backward z "Bwd | " of
                    (Unsorted _) -> pure Unsorting
                    prev -> modify succ' >> forward (k ()) prev
  where
    go = pure . Sorting
    fwd' = forward user
    pred' (0,n) = (0,n)
    pred' (x,n) = (pred x,n)
    succ' (x,n) = (succ x,n)
    -- fwd' x = (trace "DESCEND" (forward user) x) <* trace "ASCEND" (pure ())

shift x xs = (init', last')
  where
    nonempty = x :| xs
    init' = NE.init nonempty
    last' = NE.last nonempty

backward :: SortState Text -> String -> SortState Text
backward zzz f = trace (f ++ show zzz) $ case zzz of
    Sorted [] -> error "Good jorb"
    Sorted [x] -> Unsorted (val x :| [])
    Sorted (x:xs) -> case unmerge [] [] last' of
        Right (l, r) -> bwd (Merging l r init')
        Left a -> unsorted a
      where
        (init', last') = shift x xs

    Merging l r [] -> bwd (SortingRight l (Sorted r))
    Merging l r (x:xs) -> case unmerge l r last' of
        Right (l', []) -> bwd (Merging l' r init')
        Right ([], r') -> bwd (Merging l r' init')
        Right (l', r') -> Merging l' r' init'
        Left _ -> error "wat"
      where
        (init', last') = shift x xs

    SortingRight ls (Unsorted rs) -> bwd (SortingLeft (Sorted ls) rs)
    SortingRight ls r -> case bwd' r of
        Unsorted rs' -> bwd (SortingLeft (Sorted ls) rs')
        newR@Merging{} -> SortingRight ls newR
        newR -> bwd (SortingRight ls newR)

    SortingLeft (Unsorted ls) rs -> Unsorted (ls <> rs)
    SortingLeft l rs -> case bwd' l of
        Unsorted ls' -> Unsorted (ls' <> rs)
        newL@Merging{} -> SortingLeft newL rs
        newL -> bwd (SortingLeft newL rs)

    Unsorted _ -> error "backward Unsorted"

  where
    unsorted = Unsorted . (:| [])
    bwd' = flip backward ">>> | "
    -- bwd' = backward
    bwd = flip backward "... | "

unmerge l r (S a (L p)) = Right (S a p : l, r)
unmerge l r (S a (R p)) = Right (l, S a p : r)
unmerge _ _ (S a B) = Left a

-- ##
-- ## operations we permit the user
-- ##

data UserI a where
    GetNextStep :: UserI (Int, Int, Text, Text)
    Compare     :: Ordering -> UserI ()
    Rewrite1    :: Text -> UserI ()
    Rewrite2    :: Text -> UserI ()
    Undo        :: UserI ()

instance Show a => Show (UserI a) where
    show GetNextStep = "GetNextStep"
    show (Compare a) = "Compare " ++ show a
    show (Rewrite1 a) = "Rewrite1 " ++ show a
    show (Rewrite2 a) = "Rewrite2 " ++ show a
    show Undo = "Undo"


type User m a = ProgramT UserI m a

getNextStep = singleton GetNextStep

-- Here's a "program".
userCompare :: MonadIO m => User m TextSort
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
