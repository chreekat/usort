{-|
 - usort!
 -
 - In order to undo, all previous program states will be pushed to a stack. O(n
 - * (n lg n)) storage, but if n is large enough to matter when *you* are the
 - sort function, you have bigger problems.
 -
 - I'll use an iterative merge sort so all state is easy to get at and think
 - about.
 -
 -}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module USort where

import Control.Monad.Fix
import Control.Applicative
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Pretty.Simple

-- | Decisions, decisions.
data Choice = L | R
    deriving (Eq, Show)

-- | Merge actions (things a user may do)
data Action a = Choose Choice | Delete Choice | Edit Choice a | Undo
    deriving (Eq, Show, Generic)

-- | Data relevant for the UI, but not the merge itself.
data DisplayState = DisplayState
    { numActs :: Int
    , estActs :: Int
    }
    deriving (Eq, Show, Generic)

-- | In the midst of a merge, this is the state to act upon.
data MergeState a = MergeState
    { _acc :: [a]
    -- ^ accumulator for current merge
    , _left :: NonEmpty a
    -- ^ left workspace
    , _right :: NonEmpty a
    -- ^ right workspace
    , _rest :: [NonEmpty a]
    -- ^ lists left to process
    , _display :: DisplayState
    , _preCmps :: PreCmp a
    -- ^ Record previous comparisons
    --
    -- We need this when optimizing for mostly-sorted lists.
    }
    deriving (Eq, Show, Generic)

-- If a value is in the set, then the key beats the value.
newtype PreCmp a = PreCmp (Map a (Set a))
    deriving (Eq, Show, Generic)

noCmp :: PreCmp a
noCmp = PreCmp (Map.empty)

-- Assume 'Choose R', so that the second element beats the first.
--
-- If the reverse truth is already in the system, return the original list. We
-- don't do error checking because this should only happen when creating
-- Arbitrary PreCmps. (Famous last words.)
stoRCmp :: Ord a => a -> a -> PreCmp a -> PreCmp a
stoRCmp l r p@(PreCmp m)
    | reCmp l r p == Just L = p
    | otherwise =
        let m' = Map.singleton r (Set.singleton l)
        in PreCmp (Map.unionWith Set.union m m')

-- (Re) compare two elements, using the pre-compare map.
reCmp :: Ord a => a -> a -> PreCmp a -> Maybe Choice
reCmp l r (PreCmp m) =
    -- l beats r if l points to a set and r is in it.
    --
    -- r beats l if r points to a set and l is in it.
    --
    -- It should never be the case that both these things are true. We could
    -- make a smart constructor if we so desired...
    --
    -- Anyway,
    let a `beats` b = fromMaybe False (fmap (Set.member b) (Map.lookup a m))
        chooseL = if l `beats` r then Just L else Nothing
        chooseR = if r `beats` l then Just R else Nothing
    in chooseL <|> chooseR

-- | Holds the new history and the next merge state.
data ActResult a
    = ActResult
    { newHistory :: [MergeState a]
    , result :: Either [a] (MergeState a)
    }
    deriving (Eq, Show)

-- | Process given action given a history and current state.
processAct
    :: Ord a
    => [MergeState a] -- ^ history
    -> MergeState a -- ^ current
    -> Action a -- ^ to be processed
    -> ActResult a

processAct history st@(MergeState acc (_:|ls) (r:|rs) rest dsp cmp) (Delete L)
    = ActResult
        (st : history)
        (findNextMerge acc ls (r:rs) rest dsp cmp)
processAct history st@(MergeState acc (l:|ls) (_:|rs) rest dsp cmp) (Delete R)
    = ActResult
        (st : history)
        (findNextMerge acc (l:ls) rs rest dsp cmp)

processAct history st@(MergeState _ (_:|ls) _ _ _ _) (Edit L new)
    = ActResult
        (st : history)
        (Right st { _left = new :| ls })
processAct history st@(MergeState _ _ (_:|rs) _ _ _) (Edit R new)
    = ActResult
        (st : history)
        (Right st { _right = new :| rs })

processAct [] state Undo = ActResult [] (Right state)
processAct (s:ss) _ Undo = ActResult ss (Right s)

-- Override: mostly sorted input on first pass
processAct
    history st@(MergeState acc (l:|[]) (r:|[]) ((f:|[]):fs) dsp cmp) (Choose L)
    = ActResult
        (st : history)
        (findNextMerge (l : acc) [r] [f] fs (succCnt dsp) cmp)
-- Break on out-of-order elems.
processAct history st@(MergeState acc (l:|[]) (r:|[]) rest dsp cmp) (Choose R) =
    let newCmp = stoRCmp l r cmp
    in ActResult
        (st : history)
        (findNextMerge (l : acc) [] [] ((r:|[]) : rest) (succCnt dsp) newCmp)

processAct history st@(MergeState acc (l:|ls) rs rest dsp cmp) (Choose L)
    = ActResult
        (st : history)
        (findNextMerge (l : acc) ls (toList rs) rest (succCnt dsp) cmp)
processAct history st@(MergeState acc ls (r:|rs) rest dsp cmp) (Choose R)
    = ActResult
        (st : history)
        (findNextMerge (r : acc) (toList ls) rs rest (succCnt dsp) cmp)

succCnt :: DisplayState -> DisplayState
succCnt (DisplayState c s) = DisplayState (succ c) s

-- | Find the next state that needs a merge action, or abort with the final
-- list.
findNextMerge
    :: Ord a
    => [a] -- ^ accumulator for current merge (reverse order)
    -> [a] -- ^ left merge workspace
    -> [a] -- ^ right merge workspace
    -> [NonEmpty a] -- ^ lists that have yet to be merged
    -> DisplayState -- ^ the new displayState to use
    -> PreCmp a
    -> Either [a] (MergeState a)
findNextMerge w x y z d cmp = fix f w x y z
    where
    -- start populating workspace
    f nxt [] [] [] (q:rest) = nxt [] (toList q) [] rest
    -- finish populating workspace
    f _ [] (l:ls) [] (q:rest) = Right (MergeState [] (l:|ls) q rest d cmp)
    -- the final merge
    f _ acc [] [] [] = Left (reverse acc)
    -- clean out remaining in left
    f nxt acc l@(_:_) [] rest = nxt (reverse l ++ acc) [] [] rest
    -- clean out remaining in right
    f nxt acc [] r@(_:_) rest = nxt (reverse r ++ acc) [] [] rest
    f nxt acc (l:ls) (r:rs) rest =
        -- Check for PreCmps
        case reCmp l r cmp of
            Just L -> nxt (l : acc) ls (r:rs) rest
            Just R -> nxt (r : acc) (l:ls) rs rest
            -- lo, an actual merge
            Nothing -> Right (MergeState acc (l:|ls) (r:|rs) rest d cmp)
    -- finalize last merge
    f nxt (a:as) [] [] rest = nxt [] [] [] (rest ++ [NE.reverse (a:|as)])

-- | Under the hood, generic usort impl that lets me add tracing to the state.
usort'
    :: (Ord a, Monad m)
    -- :: Monad m
    => (MergeState a -> MergeState a)
    -> (MergeState a -> m (Action a)) -- ^ Produces an Action
    -> [a] -- ^ Input list
    -> m [a]
usort' fn getAct xs =
    fix f
        (ActResult
            []
            (findNextMerge [] [] [] (map (:|[]) xs) (DisplayState 0 est) (PreCmp Map.empty)))
    where
    est = round (num * log num)
    num :: Double = fromIntegral (length xs)
    f _ (ActResult _ (Left final)) = pure final
    f nxt (ActResult h (Right state))
        = getAct (fn state) >>= (nxt . processAct h state)

-- | Sorts the input, given an action that produces 'Action's!
usort
    :: (Ord a, Monad m)
    => (MergeState a -> m (Action a)) -- ^ Produces an Action
    -> [a] -- ^ Input list
    -> m [a]
usort = usort' id

-- | Debug-enabled usort
dsort
    :: (Monad m, Ord a, Show a)
    => (MergeState a -> m (Action a)) -- ^ Produces an Action
    -> [a] -- ^ Input list
    -> m [a]
dsort = usort' pTraceShowId

realCompare :: (Applicative f, Ord a) => MergeState a -> f (Action a)
realCompare (MergeState _ (l:|_) (r:|_) _ _ _)
    = pure $ Choose $ if l <= r then L else R
