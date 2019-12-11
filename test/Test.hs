{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import GHC.Generics
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Functor.Identity
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import USort
import SplitItems

deriving instance Generic MergeState
deriving instance Generic Action

instance Arbitrary MergeState where
    arbitrary =
        MergeState <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> scale (min 20) arbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = fmap (NE.fromList . getNonEmpty) arbitrary
    shrink = fmap NE.fromList . filter (not . null) . shrinkList shrink . NE.toList

instance Arbitrary Text where
    -- | An alphabetic string of up to 7 charaters
    arbitrary = T.pack <$> scale (min 7) (listOf1 (elements ['a'..'z']))
    -- | Shrink to ""
    shrink = tail . T.tails

instance Arbitrary Action where
    arbitrary = oneof
        [ Choose <$> arbitrary
        , Delete <$> arbitrary
        , Edit <$> arbitrary <*> arbitrary
        , pure Undo
        ]
    shrink = genericShrink

instance Arbitrary Choice where
    arbitrary = elements [L, R]
    -- shrink to L
    shrink R = [L]
    shrink L = []

main :: IO ()
main = defaultMain tests

-- | A state that has at least two actions remaining, allowing for testing undo.
newtype TwoActions = TwoActions MergeState
    deriving (Eq, Show, Generic)

instance Arbitrary TwoActions where
    arbitrary =
        fmap TwoActions
            $ MergeState
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> scale (min 20) (getNonEmpty <$> arbitrary)
    shrink = genericShrink

tests :: TestTree
tests = testGroup
    "tests"
    [ testGroup
        "findNextMerge"
        [ testCase "empty" $ findNextMerge [] [] [] [] @?= Left []
        , testCase "single" $ findNextMerge [] [] [] ["x" :| []] @?= Left ["x"]
        , testCase "weirdA" $ findNextMerge ["x"] [] [] [] @?= Left ["x"]
        , testCase "weirdL" $ findNextMerge [] ["x"] [] [] @?= Left ["x"]
        , testCase "weirdR" $ findNextMerge [] [] ["x"] [] @?= Left ["x"]
        , testCase "lastL" $ findNextMerge ["x"] ["y"] [] [] @?= Left ["x", "y"]
        , testCase "lastR" $ findNextMerge ["x"] [] ["y"] [] @?= Left ["x", "y"]
        , testProperty "ready" $ \a r ->
            findNextMerge a ["x"] ["y"] r
                == (Right $ MergeState a ("x" :| []) ("y" :| []) r)
        , testProperty "lastMergeL" $ \(NonEmpty a) r ->
            findNextMerge a ["x"] [] [r]
                == Right (MergeState [] r (NE.reverse ("x" :| a)) [])
        , testProperty "lastMergeR" $ \(NonEmpty a) r ->
            findNextMerge a [] ["x"] [r]
                == Right (MergeState [] r (NE.reverse ("x" :| a)) [])
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
              $ \xs -> Identity (sort xs) == usort realCompare xs
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
              (snd . unResult -> Right step1) =
                  processAct [] initState (Choose L) -- e < f
              (snd . unResult -> Right step2) =
                  processAct [] step1 (Choose L) -- f < g
              (snd . unResult -> Right step3) =
                  processAct [] step2 (Choose R) -- g > a
              (snd . unResult -> Right step4) =
                  processAct [] step3 (Choose L) -- a < b
              (snd . unResult -> Right step5) =
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
    ]

propUndo :: [MergeState] -> TwoActions -> Gen Bool
propUndo h (TwoActions st) = do
    act <- oneof
        [ Choose <$> arbitrary
        , Delete <$> arbitrary
        , Edit <$> arbitrary <*> arbitrary
        ]
    let ActResult (newHist, (Right newState)) = processAct h st act
        ActResult (h', Right st') = processAct newHist newState Undo
    pure $ h == h' && st == st'

propEditL, propEditR :: [MergeState] -> MergeState -> Text -> Bool
propEditL h st@(MergeState _ (_:|ys) _ _) x =
    let ActResult (h', Right st') = processAct h st (Edit L x)
    in h' == (st:h) && st { _stleft = x:|ys } == st'
propEditR h st@(MergeState _ _ (_:|ys) _) x =
    let ActResult (h', Right st') = processAct h st (Edit R x)
    in h' == (st:h) && st { _stright = x:|ys } == st'

-- Ok i'm tired. Could use more tests of processAct, though.

