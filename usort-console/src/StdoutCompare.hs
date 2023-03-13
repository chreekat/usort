{-# LANGUAGE OverloadedStrings #-}
module StdoutCompare (stdoutCompare) where

import USort
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import Formatting
import System.Console.Haskeline
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Compared

stdoutCompare :: MergeState Text -> IO (Action Text)
stdoutCompare m@(MergeState _ (l:|_) (r:|_) _ (DisplayState dspCnt numElem) mem _ _) = do
    printPrompt (dspCnt, numElem, element l mem, element r mem)
    c <- getResponse
    case c of
        '1' -> pure $ Choose L
        '2' -> pure $ Choose R
        'd' -> Delete <$> delItem
        'e' -> either (Edit L) (Edit R) <$> editItem l r
        'i' -> Boring <$> boringItem
        'u' -> pure Undo
        _   -> unknownCommand (stdoutCompare m)

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

boringItem :: IO Choice
boringItem = do
    putStr "Lol, which item is boring? > "
    c <- getResponse
    case c of
        '1' -> pure L
        '2' -> pure R
        _   -> unknownCommand boringItem

replaceText :: Text -> IO Text
replaceText t = do
    replaced <-
        runInputT defaultSettings
                  (getInputLineWithInitial "What instead? > " ("", T.unpack t))
    return (maybe t T.pack replaced)

printPrompt :: (Int, Int, Text, Text) -> IO ()
printPrompt (remaining, numElem, x, y) = do
    mapM_
        T.putStrLn
        [ "############################################################"
        , "############################################################"
        , sformat hdrFmt remaining estimate
        , "### [1] ####################################################"
        , ""
        , xSummary
        , ""
        , "### [2] ####################################################"
        , ""
        , ySummary
        , ""
        , "-- Or:"
        , "--   [e]dit an entry,"
        , "--   [u]ndo last comparison,"
        , "--   [d]elete an entry,"
        , "--   [i]gnore an entry"
        ]
    T.putStr "-> "
  where
    estimate = nlogn (fromIntegral numElem)
    nlogn :: Double -> Int
    nlogn n = round $ n * log n
    xSummary = shorten x
    ySummary = shorten y
    shorten s = T.append summ ellipsis
      where
        (summ, remm) = T.breakOn "\n" s
        ellipsis
            | T.null remm = ""
            | otherwise = "…"
    hdrFmt = "##### (" % int % " of ~" % int % ")    Which is more important?    ############"
