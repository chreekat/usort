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
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics
import qualified Data.List.NonEmpty as NE

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
    }
    deriving (Eq, Show, Generic)

-- | Holds the new history and the next merge state.
data ActResult a
    = ActResult
    { newHistory :: [MergeState a]
    , result :: Either [a] (MergeState a)
    }
    deriving (Eq, Show)

-- | Process given action given a history and current state.
processAct
    :: [MergeState a] -- ^ history
    -> MergeState a -- ^ current
    -> Action a -- ^ to be processed
    -> ActResult a

processAct history st@(MergeState acc (_:|ls) (r:|rs) rest dsp) (Delete L)
    = ActResult
        (st : history)
        (findNextMerge acc ls (r:rs) rest dsp)
processAct history st@(MergeState acc (l:|ls) (_:|rs) rest dsp) (Delete R)
    = ActResult
        (st : history)
        (findNextMerge acc (l:ls) rs rest dsp)

processAct history st@(MergeState _ (_:|ls) _ _ _) (Edit L new)
    = ActResult
        (st : history)
        (Right st { _left = new :| ls })
processAct history st@(MergeState _ _ (_:|rs) _ _) (Edit R new)
    = ActResult
        (st : history)
        (Right st { _right = new :| rs })

processAct [] state Undo = ActResult [] (Right state)
processAct (s:ss) _ Undo = ActResult ss (Right s)

-- Override: mostly sorted input on first pass
processAct
    history st@(MergeState acc (l:|[]) (r:|[]) ((f:|[]):fs) dsp) (Choose L)
    = ActResult
        (st : history)
        (findNextMerge (l : acc) [r] [f] fs (succCnt dsp))
-- Break on out-of-order elems.
processAct history st@(MergeState acc (l:|[]) (r:|[]) rest dsp) (Choose R)
    = ActResult
        (st : history)
        (findNextMerge (l : acc) [] [] ((r:|[]) : rest) (succCnt dsp))

processAct history st@(MergeState acc (l:|ls) rs rest dsp) (Choose L)
    = ActResult
        (st : history)
        (findNextMerge (l : acc) ls (toList rs) rest (succCnt dsp))
processAct history st@(MergeState acc ls (r:|rs) rest dsp) (Choose R)
    = ActResult
        (st : history)
        (findNextMerge (r : acc) (toList ls) rs rest (succCnt dsp))

succCnt :: DisplayState -> DisplayState
succCnt (DisplayState c s) = DisplayState (succ c) s

-- | Find the next state that needs a merge action, or abort with the final
-- list.
findNextMerge
    :: [a] -- ^ accumulator for current merge (reverse order)
    -> [a] -- ^ left merge workspace
    -> [a] -- ^ right merge workspace
    -> [NonEmpty a] -- ^ lists that have yet to be merged
    -> DisplayState -- ^ the new displayState to use
    -> Either [a] (MergeState a)
findNextMerge w x y z d = fix f w x y z
    where
    -- start populating workspace
    f nxt [] [] [] (q:rest) = nxt [] (toList q) [] rest
    -- finish populating workspace
    f _ [] (l:ls) [] (q:rest) = Right (MergeState [] (l:|ls) q rest d)
    -- the final merge
    f _ acc [] [] [] = Left (reverse acc)
    -- clean out remaining in left
    f nxt acc l@(_:_) [] rest = nxt (reverse l ++ acc) [] [] rest
    -- clean out remaining in right
    f nxt acc [] r@(_:_) rest = nxt (reverse r ++ acc) [] [] rest
    -- lo, an actual merge
    f _ acc (l:ls) (r:rs) rest = Right (MergeState acc (l:|ls) (r:|rs) rest d)
    -- finalize last merge
    f nxt (a:as) [] [] rest = nxt [] [] [] (rest ++ [NE.reverse (a:|as)])

-- | Under the hood, generic usort impl that lets me add tracing to the state.
usort'
    :: Monad m
    -- :: Monad m
    => (MergeState a -> MergeState a)
    -> (MergeState a -> m (Action a)) -- ^ Produces an Action
    -> [a] -- ^ Input list
    -> m [a]
usort' fn getAct xs =
    fix f
        (ActResult
            []
            (findNextMerge [] [] [] (map (:|[]) xs) (DisplayState 0 est)))
    where
    est = round (num * log num)
    num :: Double = fromIntegral (length xs)
    f _ (ActResult _ (Left final)) = pure final
    f nxt (ActResult h (Right state))
        = getAct (fn state) >>= (nxt . processAct h state)
        -- = getAct state >>= (nxt . processAct h state)

-- | Sorts the input, given an action that produces 'Action's!
usort
    :: Monad m
    => (MergeState a -> m (Action a)) -- ^ Produces an Action
    -> [a] -- ^ Input list
    -> m [a]
usort = usort' id

-- | Debug-enabled usort
dsort
    :: (Monad m, Show a)
    => (MergeState a -> m (Action a)) -- ^ Produces an Action
    -> [a] -- ^ Input list
    -> m [a]
dsort = usort' pTraceShowId

realCompare :: (Applicative f, Ord a) => MergeState a -> f (Action a)
realCompare (MergeState _ (l:|_) (r:|_) _ _)
    = pure $ Choose $ if l <= r then L else R
