{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
import Prelude as Pre

import Control.Error
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Data.List
import Data.List.NonEmpty as NE hiding (sort, map)
import Control.Monad.Operational
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

import qualified Data.Text as T

import Lib
import Sorted
import Merge

-- What are some properties I want?
-- 1. The output list should always be the same size, naturally
-- 2. The output list should be sorted, naturally
-- 3. An edited item should show up in the output. This might be hard to
-- test?
-- 4. Any number selections + the same number of undos should be
-- idempotent.
-- 5. For that matter, return should just return the list as is! :)

newtype ChaosCompare v m a = CC (CmpT v m a)

instance Arbitrary (ChaosCompare v m a) where
    arbitrary = forever $ frequency
        [(3, pure (Compare LT))
        ,(3, pure (Compare GT))
        ,(1, pure Undo)
        ,(1, (Rewrite1 . T.pack) <$> arbitrary)
        ,(1, (Rewrite2 . T.pack) <$> arbitrary)
        ]

pureCompare :: ( tInner ~ ExceptT (MergeFail a) IO
               , Ord a)
            => CmpT a tInner b
pureCompare = forever ((singleton GetNextStep) >>= obvious)
  where
    obvious (_, _, l, r) = singleton (Compare (compare l r))

runSort fn xs = do
    mvar <- newMVar (Just fn)
    runReaderT (sortFunc xs) mvar

finishSort fn xs = do
    res <- runSort fn xs
    case res of
        Left (Unsorted xs') -> finishSort fn xs'
        Right xs' -> pure xs'

prop_sameSize xs = ioProperty $ do
    (CC fn) <- generate arbitrary
    result <- finishSort fn ts
    pure (Pre.length xs == Pre.length result)
  where
    ts = map T.pack xs

prop_sorted xs = ioProperty $ do
    result <- finishSort pureCompare ts
    pure (sort ts == map val result)
  where
    ts = map T.pack xs

-- Just gonna brainstorm some tests here. Maybe start with a list, do one
-- compare that swaps two elements (I forget if it's GT or LT that would do
-- that), and then see if result is as expected? That seems incredibly
-- dull. What's the point. No, just stick with the two properties above,
-- which seem good.

main = $(defaultMainGenerator)
