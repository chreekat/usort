{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import qualified Data.ByteString as B

import USort
import SplitItems

instance Arbitrary MergeState where
    arbitrary =
        MergeState <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> scale (min 20) arbitrary

instance Arbitrary Text where
    -- | An alphabetic string of up to 7 charaters
    arbitrary = T.pack <$> scale (min 7) (listOf1 (elements ['a'..'z']))

instance Arbitrary Action where
    arbitrary = oneof
        [ Choose <$> arbitrary
        , Delete <$> arbitrary
        , Edit <$> arbitrary <*> arbitrary
        , pure Undo
        ]

instance Arbitrary Choice where arbitrary = elements [L, R]

main :: IO ()
main = defaultMain tests

-- | A state that has at least two actions remaining, allowing for testing undo.
newtype TwoActions = TwoActions MergeState
    deriving (Eq, Show)

instance Arbitrary TwoActions where
    arbitrary =
        fmap TwoActions
            $ MergeState
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> scale (min 20) (getNonEmpty <$> arbitrary)

tests :: TestTree
tests = testGroup "tests"
    [ testGroup "findNextMerge"
        [ testCase "empty"  $ findNextMerge [] [] [] [] @?= Left []
        , testCase "single" $ findNextMerge [] [] [] ["x":|[]] @?= Left ["x"]
        , testCase "weirdA" $ findNextMerge ["x"] [] [] [] @?= Left ["x"]
        , testCase "weirdL" $ findNextMerge [] ["x"] [] [] @?= Left ["x"]
        , testCase "weirdR" $ findNextMerge [] [] ["x"] [] @?= Left ["x"]
        , testCase "lastL"  $ findNextMerge ["x"] ["y"] [] [] @?= Left ["x","y"]
        , testCase "lastR"  $ findNextMerge ["x"] [] ["y"] [] @?= Left ["x","y"]
        , testProperty "ready" $
            \a r -> findNextMerge a ["x"] ["y"] r == (Right $ MergeState a ("x":|[]) ("y":|[]) r)
        , testProperty "lastMergeL" $
            \(NonEmpty a) r -> findNextMerge a ["x"] [] [r]
                == Right (MergeState [] r (NE.reverse ("x":|a)) [])
        , testProperty "lastMergeR" $
            \(NonEmpty a) r -> findNextMerge a [] ["x"] [r]
                == Right (MergeState [] r (NE.reverse ("x":|a)) [])
        ]
    , testGroup "processAct"
        [ testProperty "undo" propUndo
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
              ( (writeBinaryFile "test/golden/splitme.out")
              =<< ( (concatMap (\x -> unlines ["ITEM", x]) . items . lines)
                  <$> readFile "test/golden/splitme.txt"
                  )
              )
        ]
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

