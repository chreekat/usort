{-# LANGUAGE LambdaCase #-}

module TermboxUI where

import Termbox.Internal
import qualified Termbox

main :: IO ()
main =
  Termbox.run (\width height render poll -> loop width height render poll 0)

loop :: Int -> Int -> (Termbox.Cells -> Termbox.Cursor -> IO ()) -> IO Termbox.Event -> Int -> IO ()
loop w h render poll n = do
  -- render (string 0 0 (show w <> " " <> show h <> " " <> show n)) Termbox.NoCursor
  render (aoeu w (show n)) Termbox.NoCursor

  poll >>= \case
    Termbox.EventKey Termbox.KeyEsc -> pure ()
    Termbox.EventKey _  -> loop w h render poll (n+1)
    Termbox.EventResize w' h' -> loop w' h' render poll n

string :: Int -> Int -> String -> Termbox.Cells
string col row =
  foldMap (\(i, c) -> Termbox.set (col + i) row (Termbox.Cell c 0 0)) . zip [0..]

aoeu w n =
    let saeo = length n
        right = (w - 2 - saeo) `div` 2
        left = w - 2 - saeo - right
        linee = replicate `flip` '━'
    in
    string 0 0 $ "┍" <> linee left <> n <> linee right <> "┑"
{-
         0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
U+250x   ─  ━  │  ┃  ┄  ┅  ┆  ┇  ┈  ┉  ┊  ┋  ┌  ┍  ┎  ┏
U+251x   ┐  ┑  ┒  ┓  └  ┕  ┖  ┗  ┘  ┙  ┚  ┛  ├  ┝  ┞  ┟
U+252x   ┠  ┡  ┢  ┣  ┤  ┥  ┦  ┧  ┨  ┩  ┪  ┫  ┬  ┭  ┮  ┯
U+253x   ┰  ┱  ┲  ┳  ┴  ┵  ┶  ┷  ┸  ┹  ┺  ┻  ┼  ┽  ┾  ┿
U+254x   ╀  ╁  ╂  ╃  ╄  ╅  ╆  ╇  ╈  ╉  ╊  ╋  ╌  ╍  ╎  ╏
U+255x   ═  ║  ╒  ╓  ╔  ╕  ╖  ╗  ╘  ╙  ╚  ╛  ╜  ╝  ╞  ╟
U+256x   ╠  ╡  ╢  ╣  ╤  ╥  ╦  ╧  ╨  ╩  ╪  ╫  ╬  ╭  ╮  ╯
U+257x   ╰  ╱  ╲  ╳  ╴  ╵  ╶  ╷  ╸  ╹  ╺  ╻  ╼  ╽  ╾  ╿
-}
