{-# LANGUAGE LambdaCase #-}

module TermboxUI where

import Termbox.Internal
import qualified Termbox

-- FIXME: Drawing funcs should be in a Thingy/Monad that has starting
-- coordinate, width, height, among other things

main :: IO ()
main =
    Termbox.run (\width height render poll -> loop width height render poll 0)

renderUs r = r . mconcat

loop :: Int -> Int -> (Termbox.Cells -> Termbox.Cursor -> IO ()) -> IO Termbox.Event -> Int -> IO ()
loop w h r poll n = do
    renderUs r (topBorder w ("USORT " <> show n) : topHeader w h "Which is more important?" n 6) Termbox.NoCursor

    poll >>= \case
        Termbox.EventKey Termbox.KeyEsc -> pure ()
        Termbox.EventKey _  -> loop w h r poll (n+1)
        Termbox.EventResize w' h' -> loop w' h' r poll n

string :: Int -> Int -> String -> Termbox.Cells
string col row =
    foldMap (\(i, c) -> Termbox.set (col + i) row (Termbox.Cell c 0 0)) . zip [0..]

topHeader w h prompt done size =
    let
        n = fromIntegral size
        pPrompt = "│" <> prompt <> "│"
        pStatus = "│" <> show done <> " of ~" <> show (round $ n * log n) <> "│"
        pPromptL = length pPrompt
        pStatusL = length pStatus
        spacing = replicate (w - length pPrompt - length pStatus) ' '
        firstLine = pPrompt <> spacing <> pStatus
        secondLine = concat
            [ "└"
            , replicate (pPromptL - 2) '─'
            , "┘"
            , spacing
            , "└"
            , replicate (pStatusL - 2) '─'
            , "┘"
            ]
        overwrite =
            flip foldMap [pPromptL - 1, pPromptL + length spacing] $
                \w -> Termbox.set w 0 (Termbox.Cell '┯' 0 0)
    in
    [ string 0 1 firstLine
    , string 0 2 secondLine
    -- FIXME: Overwriting already-drawn characters is lame, but to stop and solve
    -- it Once And For All™ would be very Haskell of me.
    , overwrite
    ]

topBorder width title =
    let
        titleLength = length title
        right = (width - 2 - titleLength) `div` 2
        left = width - 2 - titleLength - right
        linee = replicate `flip` '━'
    in
    string 0 0 $ "┍" <> linee left <> title <> linee right <> "┑"


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
