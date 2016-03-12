{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Lib where

import Control.Monad.Operational
import Control.Monad.State
import Data.Foldable
import Data.List.NonEmpty hiding (toList, map, reverse)
import Data.Semigroup
import Data.Text (Text)
import Formatting hiding (left, right)
import System.Console.Haskeline hiding (replacement)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- import Debug.Trace

-- | Given a list of items, use merge sort where the sort function is YOU
-- :D
uSort :: [Text] -> IO [Text]
uSort [] = pure []
uSort (x:xs) = toList <$> sortFunc userCompare (Unsorted (x :| xs))

class ToNonEmpty m where
    toList' :: m a -> NonEmpty a

instance ToNonEmpty NonEmpty where
    toList' = id

-- | Does a weird even/odd thing, but whatevs
half :: a -> a -> [a] -> (NonEmpty a, NonEmpty a)
half x x' = \case
    [] -> (x :| [], x' :| [])
    [x''] -> (x :| [], x' :| [x''])
    (z:z':zs) -> let (h,h') = half z z' zs
                 in (x <| h, x' <| h')

data Merge a = PickLeft a
             | PickRight a
             | SkipLeft (NonEmpty a)
             | SkipRight (NonEmpty a)
             deriving Show

instance ToNonEmpty Merge where
    toList' = \case
        PickLeft a -> a :| []
        PickRight a -> a :| []
        SkipLeft ne -> ne
        SkipRight ne -> ne

instance Foldable Merge where
    foldMap f = \case
        PickLeft a -> f a
        PickRight a -> f a
        SkipLeft (a :| as) -> mconcat (f a : map f as)
        SkipRight (a :| as) -> mconcat (f a : map f as)

data SortState a
        = Unsorted (NonEmpty a)
        | SortingLeft
        | SortingRight
        | Merging

        -- | Unsorted (NonEmpty a)
        -- | SortingLeft (SortTree a) (NonEmpty (Merge a)) (NonEmpty a)
        -- | SortingRight (SortTree a) (SortTree a) (NonEmpty (Merge a)) (NonEmpty a)
        -- | Merging (SortTree a) (SortTree a) (NonEmpty (Merge a))

data SortTree a
        = Single a
        | STree
            { sortL :: SortTree a
            , sortR :: SortTree a
            , merges :: NonEmpty (Merge a)
            }
        deriving Show

instance ToNonEmpty SortTree where
    toList' = \case
        Single a -> a :| []
        STree _ _ ms -> sconcat (fmap toList' (NE.reverse ms))

instance Foldable SortTree where
    foldMap f = \case
        Single a -> f a
        STree _ _ ms -> foldMap f (sconcat (fmap toList' (NE.reverse ms)))

sortFunc :: User IO [Text] -> SortState Text -> IO (NonEmpty Text)
sortFunc = undefined

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

editItem :: Text -> Text -> IO (Either Text Text)
editItem x y = do
    putStr "Which item? > "
    c <- getResponse
    case c of
        '1' -> Left  <$> replaceText x
        '2' -> Right <$> replaceText y
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

