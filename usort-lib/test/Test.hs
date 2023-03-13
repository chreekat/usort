{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-type-defaults #-}

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Data.Bifunctor
import Data.Functor.Identity
import Data.Foldable
import Data.List (sort, nub)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T

import USort
import USort.SplitItems
import USort.Compared

-- | Shrinks to 0s
instance Arbitrary DisplayState where
    arbitrary = DisplayState <$> arbitrary <*> arbitrary
    shrink (DisplayState 0 0) = []
    shrink ds@(DisplayState _ _) = DisplayState 0 0 : genericShrink ds

deriving instance (Ord a, Arbitrary a) => Arbitrary (ElementMap a)
deriving instance (Ord val, Arbitrary val, Arbitrary cmp) => Arbitrary (Compared val cmp)

listOf2 g = do
    g1 <- g
    gs <- listOf1 g
    pure (g1:gs)

fromRight (Right x) = x
fromRight _ = error "fromRight: not Right lol"

-- | Size parameter is taken to mean "order of the number of elements left to be
-- sorted"
instance (Arbitrary a, Eq a, Ord a) => Arbitrary (MergeState a) where
    arbitrary = do
        initState <- fromRight . firstCmp <$> listOf2 arbitrary
        choices <- fmap Choose <$> scale (\n -> let n' = fromIntegral n in max 0 (round (log n' * n'))) (listOf arbitrary)
        pure $ foldr (\choice state0 -> case processAct [] state0 choice of
            ActResult _ (Right state) -> state
            -- discard excess actions
            ActResult _ (Left _) -> state0) initState choices

    shrink x = shrinkToEmpty x ++ genericShrink x
      where
        shrinkToEmpty (MergeState [] (_:|[]) (_:|[]) [] (DisplayState 0 0) _ _ []) = []
        shrinkToEmpty (MergeState _ (l:|_) (r:|_) _ _ _ _ _)
            = [MergeState [] (l:|[]) (r:|[]) [] (DisplayState 0 0) mempty mempty []]

-- | A state that has at least two actions remaining, allowing for testing undo.
--
-- Ensuring "at least two actions" just means there's at least one element in
-- rest, since there's always at least one action left: comparing the heads of
-- left and right.
newtype TwoActions a = TwoActions (MergeState a)
    deriving (Eq, Show)

instance (Arbitrary a, Ord a, Eq a) => Arbitrary (TwoActions a) where
    arbitrary
        = fmap (TwoActions . clearCmps) (arbitrary `suchThat` (not . null . _rest))
            -- PreCmps skew the number of remaining actions. I am not sure if
            -- it is sound to just remove all remaining actions, but I'm not
            -- smart enough to figure it out right now.
            where clearCmps st = st { _preCmps = mempty }
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
        , Boring <$> arbitrary
        , pure Nop
        ]
    shrink x = shrinkToNop x ++ genericShrink x
      where
        shrinkToNop Nop = []
        shrinkToNop _ = [Nop]

instance Arbitrary Choice where
    arbitrary = elements [L, R]
    -- Shrink to L
    shrink R = [L]
    shrink L = []

main :: IO ()
main = defaultMain tests

data Quick a = Quick [a] [a] [a] [NonEmpty a] [a]
    deriving (Eq, Show, Functor, Foldable, Traversable)

tests :: TestTree
tests = testGroup
    "tests"
    [ testGroup
        "findNextCmp"
        (let nullDsp = DisplayState 0 0
             findNextCmp' a b c d e = findNextCmp @Int a' b' c' d' nullDsp mem mempty e'
                where (Quick a' b' c' d' e', mem) = elementMap (Quick a b c d e)
             findNextCmp'' a b c d e = (q, findNextCmp @Int a' b' c' d' nullDsp mem mempty e')
                where (q@(Quick a' b' c' d' e'), mem) = elementMap (Quick a b c d e)
        in
        [ testCase "empty" $ findNextCmp' [] [] [] [] [] @?= Left []
        , testCase "single" $ findNextCmp' [] [] [] [42 :| []] [] @?= Left [42]
        , testCase "weirdA" $ findNextCmp' [42] [] [] [] [] @?= Left [42]
        , testCase "weirdL" $ findNextCmp' [] [42] [] [] [] @?= Left [42]
        , testCase "weirdR" $ findNextCmp' [] [] [42] [] [] @?= Left [42]
        , testCase "lastL" $ findNextCmp' [42] [47] [] [] [] @?= Left [42, 47]
        , testCase "lastR" $ findNextCmp' [42] [] [47] [] [] @?= Left [42, 47]
        -- When there's something in both left and right, we expect no changes
        -- to merge state, since everything is already ready for a comparison to
        -- occur.
        , testProperty "ready" $ \acc rest boring ->
            case findNextCmp'' acc [42] [47] rest boring of
                (Quick acck [l1k] [r1k] restk boringk
                 , Right (MergeState {..}))
                 -> acck === _acc
                    .&. _left === (l1k:|[])
                    .&. _right === (r1k:|[])
                    .&. _rest === restk
                    .&. _boring === boringk
                _ -> property False
        -- given an accumlator, 1 list in the rest, and the borings, when there is
        -- one thing in left but nothing in right, we expect the new state to be:
        -- acc: empty
        -- left: the list in rest
        -- right: the orig acc reversed ++ [the one thing in left]
        -- rest: empty
        -- boring: same as before
        , testProperty "lastMergeL" $ \(NonEmpty acc) rest boring ->
            case findNextCmp'' acc [42] [] [rest] boring of
                ( Quick acck [l1k] [] [restk] boringk
                  , Right (MergeState {..}))
                    -> _acc === []
                        .&. _left === restk
                        .&. toList _right === reverse acck ++ [l1k]
                        .&. _boring === boringk
                x -> property False
        -- Similar to above, but there is nothing in left and one thing in
        -- right. We expect the same result.
        , testProperty "lastMergeR" $ \(NonEmpty acc) rest boring ->
            case findNextCmp'' acc [] [42] [rest] boring of
                ( Quick acck [] [r1k] [restk] boringk
                  , Right (MergeState {..}))
                    -> _acc === []
                        .&. _left === restk
                        .&. toList _right === reverse acck ++ [r1k]
                        .&. _boring === boringk
                x -> property False
        ])
    , testGroup
        "processAct"
        [ testProperty "undo"  propUndo
        , testProperty "editL" propEditL
        , testProperty "editR" propEditR
        ]
    , testGroup
        "sort"
        [ testProperty "realCompare"
              -- In the "real" world, we don't have duplicates. The current
              -- PreCmp implementation relies on this fact. That's kinda lame,
              -- but oh well.
            $ \(Sorted (xs :: [Int])) ->
                let xs' = nub xs
                in Identity xs' === usort realCompare xs'
        -- Before PreCmp, a reverse-ordered list would loop infinitely. Nice to
        -- check that now. :)
        , testProperty "reverse ordered list"
            $ \(Sorted (xs :: [Int])) ->
                let xs' = nub xs
                in Identity xs' === usort realCompare (reverse xs')
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
                findNextCmp @Int
                    []
                    []
                    []
                    (NE.group [5,6,7,1,2,3])
                    (DisplayState 0 0)
                    mempty
                    mempty
                    []
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
                    (DisplayState 0 0)
                    mempty
                    mempty
                    []
                )
                initState
            assertEqual
                "step1"
                (MergeState
                    [5]
                    (6:|[])
                    (7:|[])
                    (NE.group [1,2,3])
                    (DisplayState 1 0)
                    mempty
                    (observe 5 6 L mempty)
                    []
                )
                step1
            assertEqual
                "step2"
                (MergeState
                    [6, 5]
                    (7:|[])
                    (1:|[])
                    (NE.group [2,3])
                    (DisplayState 2 0)
                    mempty
                    (observe 6 7 L (observe 5 6 L mempty))
                    []
                )
                step2
            assertEqual
                "step3"
                (MergeState
                    []
                    (1:|[])
                    (2:|[])
                    [ 3:|[]
                    , 5:|[6,7]
                    ]
                    (DisplayState 3 0)
                    mempty
                    (observe 7 1 R (observe 6 7 L (observe 5 6 L mempty)))
                    []
                )
                step3
            assertEqual
                "step4"
                (MergeState
                    [1]
                    (2:|[])
                    (3:|[])
                    [ 5:|[6,7]
                    ]
                    (DisplayState 4 0)
                    mempty
                    (observe 1 2 L (observe 7 1 R (observe 6 7 L (observe 5 6 L mempty))))
                    []
                )
                step4
            assertEqual
                "step5"
                (MergeState
                    []
                    (5:|[6,7])
                    (1:|[2,3])
                    []
                    (DisplayState 5 0)
                    mempty
                    (observe 2 3 L (observe 1 2 L (observe 7 1 R (observe 6 7 L (observe 5 6 L mempty)))))
                    []
                )
                step5
    {-
     - Bad test. I added 'boring' and the property in qusetion changed.

    , testGroup
        "counts comparisons correctly"
        [ testProperty "only counts Choose" propCountChoose
          -- * could be cool to statistically check count ~ n lg n
          -- * if we account for delete, some tests should check it makes sense
        ]
    -}

    , testGroup
        "Compared"
        [ testProperty "observe idempotent" $ \(i :: Int) j (o :: Choice) c -> i /= j ==> (let new = observe i j o c in recompare i j new === Just o)
        , testProperty "inverted observe idempotent" $ \(i :: Int) j (o :: Choice) c -> i /= j ==> (let new = observe i j o c in recompare j i new === Just (invert o))
        , testProperty "order matters" $ \(i :: Int) j (o :: Choice) -> i /= j ==> recompare j i (observe i j o mempty) === Just (invert o)
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
propEditL h st@(MergeState _ (y:|_) _ _ _ _ _ _) x =
    let ActResult h' (Right st') = processAct h st (Edit L x)
    in property $ h' == (st:h) && element y (_memory st') == x

propEditR h st@(MergeState _ _ (y:|_) _ _ _ _ _) x =
    let ActResult h' (Right st') = processAct h st (Edit R x)
    in property $ h' == (st:h) && element y (_memory st') == x

-- Ok i'm tired. Could use more tests of processAct, though.
