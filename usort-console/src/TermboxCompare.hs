{-# LANGUAGE LambdaCase #-}

module TermboxCompare where

import Termbox.Internal
import qualified Termbox

import USort
import USort.Console (cliItems)

-- FIXME: Drawing funcs should be in a Thingy/Monad that has starting
-- coordinate, width, height, among other things

termboxCompare :: Q -> MergeState Text -> IO (Action Text)
termboxCompare = undefined -- wait

-- New strategy. findNextCmp really is the entrypoint to usort, so we'll just
-- use it. We can bring up and tear down the termbox ui whenever we dang well
-- please.
-- 
-- So, we start the same way we already start in Main.

main :: IO ()
main = do
    Items b is <- cliItems
    let res = firstCmp is
    sorted <- case res of
        Left js -> pure js
        Right mergeSt -> termboxSort mergeSt
    putStrLn (joinItems (Items b sorted))


termboxSort st = do
    -- Show the cmp page
    -- If user makes a selection, find next cmp and loop
    -- If user does one of the things that doesn't require tearing down termbox,
    -- just.. don't do that.
    -- If the user *does* choose something that requires tearing down termbox
    -- (which is just edit, so you can go to $EDITOR), then bail out with that
    -- case. That means we need a new case for returns from the termbox: done
    -- with a list, or need-to-edit with the thing that needs editing.

    Termbox.run (\width height render poll -> loop width height render poll 0)

renderUs r = r . mconcat

loop :: Int -> Int -> (Termbox.Cells -> Termbox.Cursor -> IO ()) -> IO Termbox.Event -> Int -> IO ()
loop w h r poll n = do
    (renderUs r)
        [ topBorder w ("USORT IT!")
        , topHeader w h "Which is more important? Press 1 or 2." n 6
        , scrutinees w h "ab" "This is item 2\n    multiline"
        , bottomBar w h
        ]
        Termbox.NoCursor

    poll >>= \case
        Termbox.EventKey Termbox.KeyEsc -> pure ()
        Termbox.EventKey _  -> loop w h r poll (n+1)
        Termbox.EventResize w' h' -> loop w' h' r poll n
        _ -> loop w h r poll n

string :: Int -> Int -> String -> Termbox.Cells
string col row =
    foldMap (\(i, c) -> Termbox.set (col + i) row (Termbox.Cell c 0 0)) . zip [0..]

boxen :: Int -> Int -> [String] -> Termbox.Cells
boxen col row = foldMap (\(r', s) -> string col r' s) . zip [row ..]

topBorder w title =
  let
    titleLength = length title
    right = (w - 2 - titleLength) `div` 2
    left = w - 2 - titleLength - right
    linee = replicate `flip` '━'
  in
    string 0 0 $ "┯" <> linee left <> title <> linee right <> "┯"

topHeader w h prompt done size =
  let
    n = fromIntegral size
    pPrompt = "│" <> prompt <> "│"
    pStatus = "│Comparison " <> show done <> " of ~" <> show (round $ n * log n) <> "│"
    pPromptL = length pPrompt
    pStatusL = length pStatus
    spacing = replicate (w - length pPrompt - length pStatus) ' '
    firstLine = pPrompt <> spacing <> pStatus
    secondLine = concat
        [ "└"
        , replicate (pPromptL - 2) '─'
        , "┘"
        , spacing
        , "╰"
        , replicate (pStatusL - 2) '─'
        , "╯"
        ]
    overwrite =
        flip foldMap [pPromptL - 1, pPromptL + length spacing] $
            \w -> Termbox.set w 0 (Termbox.Cell '┯' 0 0)
  in mconcat
    [ string 0 1 firstLine
    , string 0 2 secondLine
    -- FIXME: Overwriting already-drawn characters is lame, but to stop and solve
    -- it Once And For All™ would be very Haskell of me.
    , overwrite
    ]

scrutinees :: Int -> Int -> String -> String -> Termbox.Cells
scrutinees w h i1 i2 = 
  let
    h0 = 3
    hlast = h - 4
    half = (hlast - h0) `div` 2
  in mconcat
    [ item w h h0 half 1 i1
    , item w h (half + h0 + 1) (hlast - h0 - half - 1) 2 i2
    ]

item :: Int -> Int -> Int -> Int -> Int -> String -> Termbox.Cells
item w h h0 height num i =
  let
    button = "[" <> show num <> "]"
    buttonBox =
        [ "┌" <> replicate (length button) '─' <> "┐"
        , "│" <> button <> "│"
        ]
    bcols = length button
    is = lines i
    icols = maximum (map length is)
    irows = length is
    intersection = if icols < bcols then '┬' else '┴'
    extension = if icols < bcols
        then replicate (bcols - icols - 1) '─' <> "┘"
        else replicate (icols - bcols - 1) '─' <> "┐"
    -- TODO: Clipping/scrolling/wrapping
    itemBox =
        [ concat 
            [ "├"
            , replicate (min bcols icols) '─'
            , [intersection]
            , extension
            ]
        ]
        <> map (\i ->
            concat
                [ "│"
                , i
                , replicate (icols - length i) ' '
                , "│"
                ]
            ) is
        <> [ concat
            [ "└"
            , replicate icols '─'
            , "┘"
            ]
        ]
    in boxen 0 h0 (buttonBox <> itemBox)

bottomBar w h = boxen 0 (h-3)
    [ replicate w '─'
    , " Or: [e]dit an entry, [u]ndo last comparison, [d]elete an entry, [i]gnore an entry"
    , replicate w '─'
    ]

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
