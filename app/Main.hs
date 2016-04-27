{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (lines, getContents, putStr, unlines, concat, readFile)

import Control.Monad
import Data.Monoid ((<>))
import GHC.IO.Handle hiding (hGetContents)
import System.IO hiding (hGetContents, putStr, hPutStrLn, readFile)
import System.Environment
import Data.Text hiding (map)
import Data.Text.IO

import Sort
import Program

main :: IO ()
-- main = undefined
main = do
    as <- getArgs
    items <- lines <$> case as of
        [] -> hGetContents =<< hDuplicate stdin
        xs -> concat <$> traverse readFile xs
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    (putStr . unlines <=< fromRight) =<< retrySort userCompare items
  where
    fromRight (Right x) = pure x
    fromRight (Left x) = do
        hPutStrLn stderr "Sorting unexpectedly ended"
        pure (map ("    " <>) x)
