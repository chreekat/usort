module Main where

import qualified App
import UserCompare (userCompare)

main :: IO ()
main = App.main userCompare
