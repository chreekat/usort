{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    print =<< editItem "One" "Two"
