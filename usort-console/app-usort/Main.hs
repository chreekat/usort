module Main where

import qualified App
import UserCompare (userCompare)
import BrickCompare

main :: IO ()
main = App.main userCompare
