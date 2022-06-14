module BrickCompare (main) where

import Brick
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Graphics.Vty

import USort



main :: IO ()
main = defaultMain
    (App
        (const [delWidget, cmpWidget (4,6) fi se])
        neverShowCursor
        (\s -> const (halt s))
        pure
        (const $ attrMap (white `on` black) []))
    ()



fi = "blah blah some stuff\n    multi line, why not"
se = "Another thing lol"

cmpWidget :: (Int, Int) -> String -> String -> Widget ()
cmpWidget (i,t) a b =
    withBorderStyle unicode (cmpHeader i t <=> cmpBody a b <=> footer)

cmpHeader i t =
    let (i', t') = (show i, show t)
        counter = "Comparison " <> i' <> " of ~" <> t'
    in
    hBorderWithLabel (str "USORT")
    <=> vLimit 3
            (border (str "Which is more important? Press 1 or 2.")
            <+> fill ' '
            <+> border (str counter))

cmpObj i s = joinBorders $
    border (str $ "[" <> show i <> "]")
    <=> border (str s <+> fill ' ')

footer = 
    vLimit 3 $ border $
        padRight Max $ str "Or: [e]dit an entry, [u]ndo last comparison, [d]elete an entry, [i]gnore an entry"

cmpBody a b = cmpObj 1 a <=> cmpObj 2 b


delWidget =
    vLimit 3 $ border $
        padRight Max $ str "Delete which item? > "
