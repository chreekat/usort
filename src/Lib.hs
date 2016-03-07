{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Lib where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Operational
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Formatting
import System.Console.Haskeline
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

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

-- ##
-- ## operations we permit the user
-- ##

data UserI a where
    GetNextStep :: UserI (Int, Int, Text, Text)
    Compare :: Ordering -> UserI ()
    Rewrite1 :: Text -> UserI ()
    Rewrite2 :: Text -> UserI ()
    Undo :: UserI ()

type User m a = ProgramT UserI m a

getNextStep = singleton GetNextStep
-- compare' = singleton Compare

-- Not sure how to interpret it yet...
-- runSort :: ...


-- But we know how to use it.
userCompare :: User IO ()
userCompare = forever (getNextStep >>= doSomething)
  where
    doSomething :: (Int, Int, Text, Text) -> User IO ()
    doSomething v@(rem, est, x, y) = do
        liftIO (printPrompt (rem, est, x, y))
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
        '1' -> Left <$> replaceText x
        '2' -> Right <$> replaceText y
        _   -> unknownCommand (editItem x y)

replaceText t = do
    replacement <- runInputT defaultSettings (getInputLineWithInitial "What instead? > " ("", T.unpack t))
    return (maybe t T.pack replacement)

printPrompt :: (Int, Int, Text, Text) -> IO ()
printPrompt (remaining, estimate, x, y) = do
    T.putStrLn (sformat ("(~" % int % "/" % int % ") Which is more important?") remaining estimate)
    T.putStrLn ("-- (1) " <> x)
    T.putStrLn ("-- (2) " <> y)
    T.putStrLn "-- Or [e]dit an entry"
    T.putStrLn "-- Or [u]ndo last comparison"
    T.putStr "-> "
