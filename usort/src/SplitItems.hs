{-# LANGUAGE OverloadedStrings #-}

{- |
The point herein is to take a heirarchical list, such as

@@
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
@@

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

module SplitItems (splitItems, Items(..)) where

import Data.List
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Fix

data Items = Items { leftMargin :: Int, items :: [Text] }
    deriving (Eq, Show)

-- | Break lines into items
splitItems :: [Text] -> Items
splitItems input =
    let
        (blanks, content) =
            span (T.all isSpace) (T.transpose (makeSquare input))
        numBlank = length blanks
        assertNoTabs =
            if all (T.all (/= '\t')) blanks then () else error "Found tabs!"
        indices = case content of
            [] -> []
            (c:_) -> findIndices (not . isSpace) (T.unpack c)
        groups = splitSegments indices (T.transpose content)
    in
        Items numBlank
            (assertNoTabs
                `seq` map
                        ( T.intercalate "\n"
                        . map
                                ( T.append (T.replicate numBlank " ")
                                . T.dropWhileEnd isSpace
                                )
                        )
                        groups)

-- | Sort of like multiple splitAts; each i in 'splitSegments is' is the start
-- of a new segment.
splitSegments :: [Int] -> [a] -> [[a]]
splitSegments is xs = fix f is
  where
    f _ [] = [xs]
    f _ [i] = [drop i xs]
    f nxt (i:j:iss) = slice i (j - i) xs : nxt (j : iss)

slice :: Int -> Int -> [a] -> [a]
slice begin ct = take ct . drop begin

makeSquare :: [Text] -> [Text]
makeSquare [] = []
makeSquare ls =
    let x = maximum (map T.length ls) in map (T.justifyLeft x ' ') ls
