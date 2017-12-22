{-# LANGUAGE ViewPatterns #-}
module SplitItems (items) where

import Data.List
import Data.Char
import Control.Monad.Fix

{- 
The point herein is to take a heirarchical list, such as

    -  xxxxxxxx xxx xxxxxx xxx xxxx'x xxx xxxx xxxxxx
        -  xxxxx xx xx xxxx xxxx xxxxxx?
    -  Rxxxxxx xx
       xxxxx://xxx.xxxxxxxxx.xxxx/xx/xxxxxxxxx/xxxxx_xxxxxxxx/75#xxxx_2071
    -  xxxxxxx xxx xxxxx://xxx.xxxxxxxxx.xxxx/xx/xxxxxx/xxxxxx/42
    -  Bxxxx xxxxx xxx xxx xxxxxx I xxxx xx xx.
        -  xxx x xxxx
        -  xxxxxx
        -  xxxxxxxx
        -  xxxxxx x xxxx
        -  xxxxxx x xxxx
    -  xxxxxx xxxx xxxxxx
    -  xxxxxx xxxxxxxx xxxxxx xxx xx Sxxxxxxxx'x xxx
    -  x-x-x: xxxxxxx xxx xxxxxxxx (xxx UTCTxxx)
    -  xxxxxxx xxxxxxxxx xxxx xxxxxx
        -  xxxxx xxxx
    -  xxx xx xxxx xxx xx xxx xx/xxxx xxxx xxxxxx xxxxxx xxxxxxx xxxxx
    -  xxxxxxxx xxxxxx xxx xxxxxxx #Txxxxxx
    -  Cxxxx xx xxx xx xxxxx xxx xxx Cxxxxxxxx xx Sxxxxx xxxx xxxxxxxxx xxxxxx
    -  Cxx Sxxxxx xxxx xx xxxx x xxxxxxxx xxxx xxx xxxx xxxxxxx xxxx xx xxxx
       xxxxx xxxxxxxx?
    -  Sxxxx xxx AxxxSxxx
    -  Cxxxxx xxxxxxx xxxxxxx (x.x. xxxxxxxxxxx)
    -  xxx xxxxTxxxDxxxxxxx xxx xxx xxxxxxx

and return a list of top-level items. Whitespace should be removed and re-added
at the end.

I would eventually like to be smart about not-exactly-uncommon edge cases, such as:

1. Strip and re-add the top level and sort the subitesm when there is only one
   top-level item.
2. Prompt to sort sub lists after sorting the top list.
3. Strip and re-add whitespace in the front.

For now I'll leave these enhancements out.

Since Haskell does best with rows and not columns, I think I'll make my life
easier and reorient the character matrix.

Oh wait, we already have Data.List.transpose. Heh.

Then we just need to find the indices that start elements.
-}

items :: [String] -> [String]
items input =
    let (length -> blank, content) =
            span (all isSpace) (transpose (makeSquare input))
        indices = case content of
            [] -> []
            (c:cs) -> findIndices (not . isSpace) c
        groups = splitItems indices (transpose content)
    in  map
            (unlines . map ((replicate blank ' ' ++) . dropWhileEnd isSpace))
            groups

splitItems is xs = fix f is
  where
    f _ [] = [xs]
    f _ [i] = [drop i xs]
    f nxt (i:j:is) = slice i (j - i) xs : nxt (j : is)

slice :: Int -> Int -> [a] -> [a]
slice begin ct = take ct . drop begin

justifyLeft :: Int -> Char -> String -> String
justifyLeft x c s | x <= length s = s
                  | otherwise = s ++ replicate (x - length s) c

makeSquare [] = []
makeSquare ls = let x = maximum (map length ls) in map (justifyLeft x ' ') ls
