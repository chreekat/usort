{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (lines, getContents, putStrLn, unlines, concat, readFile)
import Lib
import GHC.IO.Handle hiding (hGetContents)
import System.IO hiding (hGetContents, putStrLn, readFile)
import System.Environment
import Data.Text
import Data.Text.IO

main :: IO ()
main = undefined
-- main = do
--     as <- getArgs
--     items <- lines <$> case as of
--         [] -> hGetContents =<< hDuplicate stdin
--         xs -> concat <$> traverse readFile xs
--     hSetBuffering stdout NoBuffering
--     hSetBuffering stdin NoBuffering
--     (putStrLn . unlines) =<< uSort items
