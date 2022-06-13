module BrickCompare (main) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import USort



main :: IO ()
main = simpleMain (cmpWidget (4,6) fi se)

fi = "blah blah some stuff\n    multi line, why not"
se = "Another thing lol"

cmpWidget :: (Int, Int) -> String -> String -> Widget ()
cmpWidget (i,t) a b =
    withBorderStyle unicode (header i t <=> body a b <=> footer)

header i t =
    let (i', t') = (show i, show t)
        counter = "Comparison " <> i' <> " of ~" <> t'
    in
    hBorderWithLabel (str "USORT")
    <=> vLimit 3
            (border (str "Which is more important? Press 1 or 2.")
            <+> fill ' '
            <+> border (str counter))

obj i s = joinBorders $
    border (str $ "[" <> show i <> "]")
    <=> border (str s <+> fill ' ')

footer = 
    vLimit 3 $ border $
        str "Or: [e]dit an entry, [u]ndo last comparison, [d]elete an entry, [i]gnore an entry"
        <+> fill ' '

body a b = obj 1 a <=> obj 2 b
