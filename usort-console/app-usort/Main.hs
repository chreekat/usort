module Main where

import qualified App
import StdoutCompare (stdoutCompare)

main :: IO ()
main = App.main stdoutCompare
