{-# LANGUAGE OverloadedStrings #-}
module Program where

import Prelude as Pre

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational
import Data.Monoid ((<>))
import Data.Text (Text)
import Formatting
import System.Console.Haskeline
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Merge

-- Here's a "program".
-- userCompare :: MonadIO m => User m TextSort
userCompare :: MrgT Text IO b
userCompare = forever (singleton GetNextStep >>= doSomething)
  where
    doSomething :: (Int, Int, Text, Text) -> MrgT Text IO ()
    doSomething v@(remain, est, x, y) = do
        liftIO (printPrompt (remain, est, x, y))
        c <- liftIO getResponse
        case c of
            '1' -> singleton (Compare LT)
            '2' -> singleton (Compare GT)
            'e' -> singleton . either (Rewrite LT) (Rewrite GT) =<< liftIO (editItem x y)
            'u' -> singleton Undo
            _   -> unknownCommand (doSomething v)

getResponse :: IO Char
getResponse = getChar <* putStrLn ""

unknownCommand :: MonadIO m => m b -> m b
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

replaceText :: MonadException m => Text -> m Text
replaceText t = do
    replaced <-
        runInputT defaultSettings
                  (getInputLineWithInitial "What instead? > " ("", T.unpack t))
    return (maybe t T.pack replaced)

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
