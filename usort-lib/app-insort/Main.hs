module Main where

import qualified App
import USort (realCompare)

main :: IO ()
main = App.main realCompare
