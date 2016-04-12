{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
import Prelude as Pre

import Control.Error
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Data.List
import Control.Monad.Operational
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH
import Data.Text (Text)

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

instance Arbitrary Text where
    arbitrary = T.pack <$> listOf (elements ['a'..'z'])

newtype ChaosCompare = CC (MrgI Text ())

instance Arbitrary ChaosCompare where
    arbitrary = CC <$> frequency
        [(3, pure (Compare LT))
        ,(3, pure (Compare GT))
        ,(1, pure Undo)
        ,(1, (Rewrite1 . T.pack) <$> arbitrary)
        ,(1, (Rewrite2 . T.pack) <$> arbitrary)
        ]

chaosCompare = forever $ do
    CC act <- liftIO $ generate arbitrary
    singleton act

pureCompare :: (Ord a) => MrgT a IO b
pureCompare = forever ((singleton GetNextStep) >>= obvious)
  where
    obvious (_, _, l, r) = singleton (Compare (compare l r))

runSort fn xs = do
    mvar <- newMVar (Just fn)
    runReaderT (sortFunc xs) mvar

finishSort fn xs = do
    mvar <- newMVar (Just fn)
    runReaderT (go xs) mvar
  where
    go xs = do
        res <- sortFunc xs
        case res of
            Left (Unsorted xs') -> go xs'
            Right xs' -> pure xs'

prop_sameSize xs = ioProperty $ do
    result <- finishSort chaosCompare xs
    pure (Pre.length xs == Pre.length result)

prop_sorted xs = ioProperty $ do
    result <- finishSort pureCompare xs
    pure (sort xs == map val result)
  where
    types = xs :: [Text]

prop_return xs = ioProperty $ do
    result <- runSort (return ()) xs
    pure (go result xs)
  where
    go result xs
        | length xs < 2 = result == Right base
        | otherwise     = result == Left (SortEnded base)
    base = map (\x -> S x B) xs
    types = xs :: [Text]

-- Just gonna brainstorm some tests here. Maybe start with a list, do one
-- compare that swaps two elements (I forget if it's GT or LT that would do
-- that), and then see if result is as expected? That seems incredibly
-- dull. What's the point. No, just stick with the two properties above,
-- which seem good.

main = $(defaultMainGenerator)
