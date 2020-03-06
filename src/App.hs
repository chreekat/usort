{-# LANGUAGE OverloadedStrings #-}
module App where

import Prelude hiding (lines, getContents, putStr, unlines, concat, readFile)

import GHC.IO.Handle hiding (hGetContents)
import System.IO hiding (hGetContents, putStr, hPutStrLn, readFile)
import System.Environment
import Data.Text hiding (map)
import Data.Text.IO
import qualified Data.Text as T

import USort
import UserCompare
import SplitItems

main :: (MergeState Text -> IO (Action Text)) -> IO ()
main sortFn = do
    as <- getArgs
    (Items b is) <- splitItems . lines <$> case as of
        [] -> do
            dup <- hDuplicate stdin
            maybe (pure ()) (hSetEncoding dup) =<< hGetEncoding stdin
            hGetContents dup
        xs -> concat <$> traverse readFile xs
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    (putStr . unlines . map (T.replicate b " " <>))
        =<< usort sortFn (map (T.drop b) is)
