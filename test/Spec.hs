{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
import Prelude as Pre

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

newtype ChaosCompare = CC { ccInstr :: MrgI Text () }

instance Arbitrary ChaosCompare where
    arbitrary = CC <$> frequency
        [(3, pure (Compare LT))
        ,(3, pure (Compare GT))
        ,(1, pure Undo)
        ,(1, (Rewrite LT . T.pack) <$> arbitrary)
        ,(1, (Rewrite GT . T.pack) <$> arbitrary)
        ]

instance Show ChaosCompare where
    show (CC x) = case x of
        Compare LT -> "LT"
        Compare _  -> "GT"
        Undo -> "Undo"
        Rewrite LT t -> "RewriteL " ++ show t
        Rewrite _  t -> "RewriteR " ++ show t
        Delete LT -> "DeleteL"
        Delete _  -> "DeleteR"

newtype NoRewriteCompare = NR { nrInstr :: MrgI Text () }

instance Arbitrary NoRewriteCompare where
    arbitrary = NR <$> frequency
        [(3, pure (Compare LT))
        ,(3, pure (Compare GT))
        ,(1, pure Undo)
        ]

instance Show NoRewriteCompare where
    show (NR x) = show (CC x)

chaosCompare :: MrgT Text (StateT (Int,Int) IO) b
chaosCompare = forever $ do
    CC act <- liftIO $ generate arbitrary
    singleton act

pureCompare :: (Ord a) => MrgT a (StateT (Int,Int) IO) b
pureCompare = forever (singleton GetNextStep >>= obvious)
  where
    obvious (_, _, l, r) = singleton (Compare (compare l r))

-- runSort' instrs = runSort (mapM singleton instrs)

runSort fn xs = runReaderT (flip evalStateT (0,0) (sortFunc xs)) =<< newMVar (Just fn)

-- | Whether or not the sort ends, input length is equal to output length
prop_sameSize xs instrs = ioProperty $ do
    result <- retrySort (mapM singleton . map ccInstr $ instrs) xs
    pure $ ppxx result xs

ppxx result xs =
    label' result
        (Pre.length xs == Pre.length (unwrap result))
  where
    _types = xs :: [Text]
    label' (Left _) = label "aborted sort"
    label' (Right _) = label "finished sort"
    unwrap (Left x) = x
    unwrap (Right x) = x

prop_chaosSameSize xs = ioProperty $ do
    result <- retrySort chaosCompare xs
    pure $ ppxx result xs

prop_sorted xs = ioProperty $ do
    Right result <- retrySort pureCompare xs
    pure (sort xs == result)
  where
    _types = xs :: [Text]

prop_rewrite xs ord' instrs nytt =
    length xs > 2 ==>
    ioProperty $ do
        result <- retrySort prg xs
        pure $ porpleskeen result
  where
    prg = mapM singleton (Rewrite ord' nytt : map nrInstr instrs)
    porpleskeen (Left x) = label "aborted sort" $ nytt `elem` x
    porpleskeen (Right x) = label "finished sort" $ nytt `elem` x

-- This test kinda sucks, since the delete should really go somewhere in
-- the middle. Or I need to figure out a way to write a chaosCompare that
-- confirms the number of deletes.
prop_delete xs ord' instrs =
    length xs > 2 ==>
    ioProperty $ do
        result <- retrySort prg xs
        pure $ porpleskeen result
  where
    prg = mapM singleton (Delete ord' : map nrInstr instrs)
    porpleskeen (Left x) =
        label "aborted sort" $
        (Pre.length xs == 1 + Pre.length x)
    porpleskeen (Right x) =
        label "finished sort" $
        (Pre.length xs == 1 + Pre.length x)

prop_return ts = ioProperty $ do
    result <- runSort (return ()) ts
    pure (go result ts)
  where
    go result xs
        | length xs < 2 = result == Right bases
        | otherwise     = result == Left (SortEnded bases)
    bases = map base ts
    _types = ts :: [Text]

-- Just gonna brainstorm some tests here. Maybe start with a list, do one
-- compare that swaps two elements (I forget if it's GT or LT that would do
-- that), and then see if result is as expected? That seems incredibly
-- dull. What's the point. No, just stick with the two properties above,
-- which seem good.

main = $(defaultMainGenerator)
