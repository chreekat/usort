{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Functor.Identity
import Data.List (sort, zipWith4)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.IO as T

import USort
import SplitItems

-- | Size parameter is taken to mean "order of the number of elements left to be
-- sorted"
instance Arbitrary a => Arbitrary (MergeState a) where
    arbitrary
        = MergeState
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> scale (round . sqrt . fromIntegral) arbitrary
            -- ^ sqrt(n) lists of size (sqrt n)

    shrink x = shrinkToEmpty x ++ genericShrink x
      where
        shrinkToEmpty (MergeState [] (_:|[]) (_:|[]) []) = []
        shrinkToEmpty (MergeState _ (l:|_) (r:|_) _)
            = [MergeState [] (l:|[]) (r:|[]) []]

-- | A state that has at least two actions remaining, allowing for testing undo.
--
-- Ensuring "at least two actions" just means there's at least one element in
-- rest, since there's always at least one action left: comparing the heads of
-- left and right.
newtype TwoActions a = TwoActions (MergeState a)
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (TwoActions a) where
    arbitrary =
        fmap TwoActions (arbitrary `suchThat` (not . null . rest))
    shrink (TwoActions ms) = map TwoActions (shrink ms)

-- | An action that is not Undo.
newtype NotUndo a = NotUndo (Action a)
    deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (NotUndo a) where
    arbitrary = fmap NotUndo (arbitrary `suchThat` notUndo)
        where
            -- Avoid Eq constraint
            notUndo Undo = False
            notUndo _ = True
    shrink (NotUndo act) = map NotUndo (shrink act)

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = fmap (NE.fromList . getNonEmpty) arbitrary

    -- This is totally some kind of cofold or whatever.
    shrink = map (NE.fromList . getNonEmpty) . shrink . NonEmpty . NE.toList

instance Arbitrary a => Arbitrary (Action a) where
    arbitrary = oneof
        [ Choose <$> arbitrary
        , Delete <$> arbitrary
        , Edit <$> arbitrary <*> arbitrary
        , pure Undo
        ]

instance Arbitrary Choice where
    arbitrary = elements [L, R]
    -- Shrink to L
    shrink R = [L]
    shrink L = []

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
    "tests"
    [ testGroup
        "findNextMerge"
        [ testCase "empty" $ findNextMerge @() [] [] [] [] @?= Left []
        , testCase "single" $ findNextMerge [] [] [] [42 :| []] @?= Left [42]
        , testCase "weirdA" $ findNextMerge [42] [] [] [] @?= Left [42]
        , testCase "weirdL" $ findNextMerge [] [42] [] [] @?= Left [42]
        , testCase "weirdR" $ findNextMerge [] [] [42] [] @?= Left [42]
        , testCase "lastL" $ findNextMerge [42] [47] [] [] @?= Left [42, 47]
        , testCase "lastR" $ findNextMerge [42] [] [47] [] @?= Left [42, 47]
        , testProperty "ready" $ \a r ->
            findNextMerge @Int a [42] [47] r
                == (Right $ MergeState a (42 :| []) (47 :| []) r)
        , testProperty "lastMergeL" $ \(NonEmpty a) r ->
            findNextMerge @Int a [42] [] [r]
                == Right (MergeState [] r (NE.reverse (42 :| a)) [])
        , testProperty "lastMergeR" $ \(NonEmpty a) r ->
            findNextMerge @Int a [] [42] [r]
                == Right (MergeState [] r (NE.reverse (42 :| a)) [])
        ]
    , testGroup
        "processAct"
        [ testProperty "undo"  propUndo
        , testProperty "editL" propEditL
        , testProperty "editR" propEditR
        ]
    , testGroup
        "sort"
        [ testProperty "realSort"
              $ \xs -> Identity (sort @Int xs) == usort realCompare xs
        ]
    , testGroup
        "splitting items"
        [ goldenVsFile
              "golden"
              "test/golden/splitme.golden"
              "test/golden/splitme.out"
              (   (T.writeFile "test/golden/splitme.out")
              =<< (   ( T.concat
                      . map (\x -> T.unlines ["ITEM", x])
                      . items
                      . splitItems
                      . T.lines
                      )
                  <$> T.readFile "test/golden/splitme.txt"
                  )
              )
        ]
    , testCase "mostly sorted input"
        $ let
              Right initState =
                  findNextMerge [] [] [] (NE.group (T.words "e f g a b c"))
              (result -> Right step1) =
                  processAct [] initState (Choose L) -- e < f
              (result -> Right step2) =
                  processAct [] step1 (Choose L) -- f < g
              (result -> Right step3) =
                  processAct [] step2 (Choose R) -- g > a
              (result -> Right step4) =
                  processAct [] step3 (Choose L) -- a < b
              (result -> Right step5) =
                  processAct [] step4 (Choose L) -- b < c
          in
              do
                  assertEqual
                      "initState"
                      ( MergeState []
                                   ("e" :| [])
                                   ("f" :| [])
                                   (NE.group (T.words ("g a b c")))
                      )
                      initState
                  assertEqual
                      "step1"
                      ( MergeState ["e"]
                                   ("f" :| [])
                                   ("g" :| [])
                                   (NE.group (T.words ("a b c")))
                      )
                      step1
                  assertEqual
                      "step2"
                      ( MergeState ["f", "e"]
                                   ("g" :| [])
                                   ("a" :| [])
                                   (NE.group (T.words ("b c")))
                      )
                      step2
                  assertEqual
                      "step3"
                      ( MergeState
                          []
                          ("a" :| [])
                          ("b" :| [])
                          ["c" :| [], NE.fromList (T.words "e f g")]
                      )
                      step3
                  assertEqual
                      "step4"
                      ( MergeState ["a"]
                                   ("b" :| [])
                                   ("c" :| [])
                                   [NE.fromList (T.words "e f g")]
                      )
                      step4
                  assertEqual
                      "step5"
                      ( MergeState []
                                   (NE.fromList (T.words "e f g"))
                                   (NE.fromList (T.words "a b c"))
                                   []
                      )
                      step5
    , testCase "counts comparisons correctly" $ pure ()
    ]

propUndo :: [MergeState Int] -> TwoActions Int -> NotUndo Int -> Bool
propUndo h (TwoActions st) (NotUndo act) =
    let ActResult newHist (Right newState) = processAct h st act
        ActResult h' (Right st') = processAct newHist newState Undo
    in h == h' && st == st'

propEditL, propEditR :: [MergeState Int] -> MergeState Int -> Int -> Property
propEditL h st@(MergeState _ (_:|ys) _ _) x =
    let ActResult h' (Right st') = processAct h st (Edit L x)
    in property $ h' == (st:h) && st { left = x:|ys } == st'
propEditR h st@(MergeState _ _ (_:|ys) _) x =
    let ActResult h' (Right st') = processAct h st (Edit R x)
    in property $ h' == (st:h) && st { right = x:|ys } == st'

-- Ok i'm tired. Could use more tests of processAct, though.
