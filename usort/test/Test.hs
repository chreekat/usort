{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-type-defaults #-}
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Functor.Identity
import Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import USort
import SplitItems

-- | Shrinks to the notable number 666
instance Arbitrary DisplayState where
    arbitrary = DisplayState <$> arbitrary <*> arbitrary
    shrink (DisplayState 666 999) = []
    shrink ds@(DisplayState _ _) = DisplayState 666 999 : genericShrink ds

instance Arbitrary (PreCmp a) where
    arbitrary = pure noPreCmp
    shrink x = []

-- | Size parameter is taken to mean "order of the number of elements left to be
-- sorted"
instance Arbitrary a => Arbitrary (MergeState a) where
    arbitrary = do
        acc <- scale (round . (/ 3) . fromIntegral) arbitrary
        left <- scale (round . (/ 3) . fromIntegral) arbitrary
        right <- scale (round . (/ 3) . fromIntegral) arbitrary
        rest <- scale (round . sqrt . fromIntegral) arbitrary
            -- ^ sqrt(n) lists of size (sqrt n)
        dsp <- do
            n <- getSize
            ct <- choose (0,n)
            let ct' = fromIntegral ct
            let est = round (ct' * log ct')
            pure (DisplayState ct est)
        pure $ MergeState acc left right rest dsp noPreCmp 

    shrink x = shrinkToEmpty x ++ genericShrink x
      where
        shrinkToEmpty (MergeState [] (_:|[]) (_:|[]) [] _ _) = []
        shrinkToEmpty (MergeState _ (l:|_) (r:|_) _ _ _)
            = [MergeState [] (l:|[]) (r:|[]) [] (DisplayState 0 0) noPreCmp]

-- | A state that has at least two actions remaining, allowing for testing undo.
--
-- Ensuring "at least two actions" just means there's at least one element in
-- rest, since there's always at least one action left: comparing the heads of
-- left and right.
newtype TwoActions a = TwoActions (MergeState a)
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (TwoActions a) where
    arbitrary
        = fmap TwoActions (arbitrary `suchThat` (not . null . _rest))
    shrink (TwoActions ms)
        = map TwoActions (filter (not . null . _rest) (shrink ms))

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
        (let nullDsp = DisplayState 0 0
             -- Need type sig to use type application below
             findNextMerge'
                :: [a]
                -> [a]
                -> [a]
                -> [NonEmpty a]
                -> Either [a] (MergeState a)
             findNextMerge' a b c d = findNextMerge a b c d nullDsp noPreCmp
        in
        [ testCase "empty" $ findNextMerge' @() [] [] [] [] @?= Left []
        , testCase "single" $ findNextMerge' [] [] [] [42 :| []] @?= Left [42]
        , testCase "weirdA" $ findNextMerge' [42] [] [] [] @?= Left [42]
        , testCase "weirdL" $ findNextMerge' [] [42] [] [] @?= Left [42]
        , testCase "weirdR" $ findNextMerge' [] [] [42] [] @?= Left [42]
        , testCase "lastL" $ findNextMerge' [42] [47] [] [] @?= Left [42, 47]
        , testCase "lastR" $ findNextMerge' [42] [] [47] [] @?= Left [42, 47]
        , testProperty "ready" $ \a r ->
            findNextMerge' @Int a [42] [47] r
                == Right (MergeState a (42 :| []) (47 :| []) r nullDsp noPreCmp)
        , testProperty "lastMergeL" $ \(NonEmpty a) r ->
            findNextMerge' @Int a [42] [] [r]
                == Right (MergeState [] r (NE.reverse (42 :| a)) [] nullDsp noPreCmp)
        , testProperty "lastMergeR" $ \(NonEmpty a) r ->
            findNextMerge' @Int a [] [42] [r]
                == Right (MergeState [] r (NE.reverse (42 :| a)) [] nullDsp noPreCmp)
        ])
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
              (   T.writeFile "test/golden/splitme.out"
              =<< (   T.concat
                      . map (\x -> T.unlines ["ITEM", x])
                      . items
                      . splitItems
                      . T.lines
                  <$> T.readFile "test/golden/splitme.txt"
                  )
              )
        ]
    , testCase "mostly sorted input" $
        let
            Right initState =
                findNextMerge @ Int
                    []
                    []
                    []
                    (NE.group [5,6,7,1,2,3])
                    (DisplayState 0 11)
                    noPreCmp
            (result -> Right step1) =
                processAct [] initState (Choose L) -- 5 < 6
            (result -> Right step2) =
                processAct [] step1 (Choose L) -- 6 < 7
            (result -> Right step3) =
                processAct [] step2 (Choose R) -- 7 > 1
            (result -> Right step4) =
                processAct [] step3 (Choose L) -- 1 < 2
            (result -> Right step5) =
                processAct [] step4 (Choose L) -- 2 < 3
        in do
            assertEqual
                "initState"
                (MergeState
                    []
                    (5:|[])
                    (6:|[])
                    (NE.group [7,1,2,3])
                    (DisplayState 0 11)
                    noPreCmp
                )
                initState
            assertEqual
                "step1"
                (MergeState
                    [5]
                    (6:|[])
                    (7:|[])
                    (NE.group [1,2,3])
                    (DisplayState 1 11)
                    noPreCmp
                )
                step1
            assertEqual
                "step2"
                (MergeState
                    [6, 5]
                    (7:|[])
                    (1:|[])
                    (NE.group [2,3])
                    (DisplayState 2 11)
                    noPreCmp
                )
                step2
            assertEqual
                "step3"
                (MergeState
                    []
                    (1:|[])
                    (2:|[])
                    [ (3:|[])
                    , (5:|[6,7])
                    ]
                    (DisplayState 3 11)
                    noPreCmp
                )
                step3
            assertEqual
                "step4"
                (MergeState
                    [1]
                    (2:|[])
                    (3:|[])
                    [ (5:|[6,7])
                    ]
                    (DisplayState 4 11)
                    noPreCmp
                )
                step4
            assertEqual
                "step5"
                (MergeState
                    []
                    (5:|[6,7])
                    (1:|[2,3])
                    []
                    (DisplayState 5 11)
                    noPreCmp
                )
                step5
    , testGroup
        "counts comparisons correctly"
        [ testProperty "only counts Choose" propCountChoose
          -- * could be cool to statistically check count ~ n lg n
          -- * if we account for delete, some tests should check it makes sense
        ]
    ]

propCountChoose :: [MergeState Int] -> TwoActions Int -> NotUndo Int -> Property
propCountChoose h (TwoActions st) (NotUndo act) =
    let ActResult _ (Right newState) = processAct h st act
        isChoice (Choose _) = True
        isChoice _ = False
    in classify (isChoice act) "Choose" $ case act of
        Choose _ -> _display newState === succCnt (_display st)
        _ -> _display newState === _display st
propUndo :: [MergeState Int] -> TwoActions Int -> NotUndo Int -> Property
propUndo h (TwoActions st) (NotUndo act) =
    let ActResult newHist (Right newState) = processAct h st act
        ActResult h' (Right st') = processAct newHist newState Undo
    in (h, st) === (h', st')

propEditL, propEditR :: [MergeState Int] -> MergeState Int -> Int -> Property
propEditL h st@(MergeState _ (_:|ys) _ _ _ _) x =
    let ActResult h' (Right st') = processAct h st (Edit L x)
    in property $ h' == (st:h) && st { _left = x:|ys } == st'
propEditR h st@(MergeState _ _ (_:|ys) _ _ _) x =
    let ActResult h' (Right st') = processAct h st (Edit R x)
    in property $ h' == (st:h) && st { _right = x:|ys } == st'

-- Ok i'm tired. Could use more tests of processAct, though.
