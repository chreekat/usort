{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (lines, getContents, putStr, unlines, concat, readFile)

import GHC.IO.Handle hiding (hGetContents)
import System.IO hiding (hGetContents, putStr, hPutStrLn, readFile)
import System.Environment
import Data.Text hiding (map)
import Data.Text.IO

import USort
import UserCompare
import SplitItems

main :: IO ()
main = do
    as <- getArgs
    is <- items . lines <$> case as of
        [] -> do
            dup <- hDuplicate stdin
            maybe (pure ()) (hSetEncoding dup) =<< hGetEncoding stdin
            hGetContents dup
        xs -> concat <$> traverse readFile xs
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    (putStr . unlines) =<< usort realCompare is
