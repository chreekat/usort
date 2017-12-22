{-# LANGUAGE OverloadedStrings #-}
module UserCompare (userCompare) where

import USort
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import Formatting
import System.Console.Haskeline
import qualified Data.Text as T
import qualified Data.Text.IO as T

userCompare :: MergeState -> IO Action
userCompare m@(MergeState _ (l:|_) (r:|_) _) = do
    printPrompt (remain, est, l, r)
    c <- getResponse
    case c of
        '1' -> pure $ Choose L
        '2' -> pure $ Choose R
        'd' -> Delete <$> delItem
        'e' -> either (Edit L) (Edit R) <$> editItem l r
        'u' -> pure Undo
        _   -> unknownCommand (userCompare m)
  where
    remain = 999
    est = 333

getResponse :: IO Char
getResponse = getChar <* putStrLn ""

unknownCommand :: IO a -> IO a
unknownCommand cont = do
    putStrLn "Unknown command. Let's try again."
    cont

delItem :: IO Choice
delItem = do
    putStr "Which item? > "
    c <- getResponse
    case c of
        '1' -> pure L
        '2' -> pure R
        _   -> unknownCommand delItem

editItem :: Text -> Text -> IO (Either Text Text)
editItem x y = do
    putStr "Which item? > "
    c <- getResponse
    case c of
        '1' -> Left  <$> replaceText x
        '2' -> Right <$> replaceText y
        _   -> unknownCommand (editItem x y)

replaceText :: MonadException m => Text -> m Text
replaceText t = do
    replaced <-
        runInputT defaultSettings
                  (getInputLineWithInitial "What instead? > " ("", T.unpack t))
    return (maybe t T.pack replaced)

printPrompt :: (Int, Int, Text, Text) -> IO ()
printPrompt (remaining, estimate, x, y) = do
    T.putStrLn (sformat hdrFmt remaining estimate)
    T.putStrLn ("-- (1)\n" <> x)
    T.putStrLn ("-- (2)\n" <> y)
    T.putStrLn "-- Or [e]dit an entry"
    T.putStrLn "-- Or [u]ndo last comparison"
    T.putStrLn "-- Or [d]elete an entry"
    T.putStr "-> "
  where
    hdrFmt = "(~" % int % "/" % int % ") Which is more important?"
