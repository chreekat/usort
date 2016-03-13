{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
import Prelude as Pre

import Control.Monad.State
import Data.List
import Data.List.NonEmpty as NE hiding (sort, map)
import Lib
import Control.Monad.Operational
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

import qualified Data.Text as T

-- What are some properties I want?
-- 1. The output list should always be the same size, naturally
-- 2. The output list should be sorted, naturally
-- 3. An edited item should show up in the output. This might be hard to
-- test?
-- 4. Any number selections + the same number of undos should be
-- idempotent.
-- 5. For that matter, return should just return the list as is! :)

instance Arbitrary (UserI ()) where
    arbitrary = frequency [(3, pure (Compare LT))
                          ,(3, pure (Compare GT))
                          ,(1, pure Undo)
                          ,(1, (Rewrite1 . T.pack) <$> arbitrary)
                          ,(1, (Rewrite2 . T.pack) <$> arbitrary)
                          ]

chaosCompare :: User (StateT (Int,Int) IO) TextSort
chaosCompare = forever goNext
  where
    goNext :: User (StateT (Int,Int) IO) ()
    goNext = do
        act <- liftIO $ generate arbitrary
        singleton act

pureCompare :: User (StateT (Int,Int) IO) TextSort
pureCompare = forever (getNextStep >>= obvious)
  where
    obvious (_, _, l, r) = singleton (Compare (compare l r))

prop_sameSize x xs = ioProperty $ do
    result <- sortFunc chaosCompare (T.pack x :| map T.pack xs)
    pure (Pre.length (x:xs) == Pre.length result)

prop_sorted x xs = ioProperty $ do
    result <- sortFunc pureCompare (t :| ts)
    pure (sort (t:ts) == result)
  where
    t = T.pack x
    ts = map T.pack xs

main = $(defaultMainGenerator)
