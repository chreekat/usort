{-|
usort!
-}

{-
In order to undo, all previous program states will be pushed to a stack. O(n
* (n lg n)) storage, but if n is large enough to matter when *you* are the
sort function, you have bigger problems.

I'll use an iterative merge sort so all state is easy to get at and think
about.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module USort where

import Control.Applicative
import Control.Monad.Fix
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import GHC.Generics
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set

import USort.Compared

import Debug.Pretty.Simple

-- | Decisions, decisions.
data Choice = L | R
    deriving (Eq, Show)

instance Invertible Choice where
    invert L = R
    invert R = L

-- | Merge actions (things a user may do)
data Action a = Choose Choice | Delete Choice | Edit Choice a | Undo | Boring Choice | Nop
    deriving (Eq, Show, Generic)

-- | Data relevant for the UI, but not the merge itself.
data DisplayState = DisplayState
    { numActs :: Int
    , numElems :: Int
    }
    deriving (Eq, Show, Generic)

succCnt :: DisplayState -> DisplayState
succCnt (DisplayState c s) = DisplayState (succ c) s

predElem :: DisplayState -> DisplayState
predElem (DisplayState c s) = DisplayState c (pred s)

-- | In the midst of a merge, this is the state to act upon.
data MergeState a = MergeState
    { _acc :: [Int]
    -- ^ accumulator for current merge
    , _left :: NonEmpty Int
    -- ^ left workspace
    , _right :: NonEmpty Int
    -- ^ right workspace
    , _rest :: [NonEmpty Int]
    -- ^ lists left to process
    , _display :: DisplayState
    , _memory :: ElementMap a
    , _preCmps :: Compared Int Choice
    -- ^ Record previous comparisons
    , _boring :: [Int]
    -- ^ Items we don't sort
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
    :: Ord a
    => [MergeState a] -- ^ history
    -> MergeState a -- ^ current
    -> Action a -- ^ to be processed
    -> ActResult a

processAct history st@(MergeState acc (_:|ls) (r:|rs) rest dsp _mem cmp b) (Delete L)
    = ActResult
        (st : history)
        (findNextCmp acc ls (r:rs) rest (predElem dsp) _mem cmp b)
processAct history st@(MergeState acc (l:|ls) (_:|rs) rest dsp _mem cmp b) (Delete R)
    = ActResult
        (st : history)
        (findNextCmp acc (l:ls) rs rest (predElem dsp) _mem cmp b)

processAct history st (Edit L new)
    = let newMem = insertEl (NE.head (_left st)) new (_memory st)
       in ActResult
        (st : history)
        (Right st { _memory = newMem })
processAct history st (Edit R new)
    = let newMem = insertEl (NE.head (_right st)) new (_memory st)
       in ActResult
        (st : history)
        (Right st { _memory = newMem })

processAct [] state Undo = ActResult [] (Right state)
processAct (s:ss) _ Undo = ActResult ss (Right s)

-- Override: mostly sorted input on first pass
processAct
    history st@(MergeState acc (l:|[]) (r:|[]) ((f:|[]):fs) dsp mem cmp b) (Choose L)
    = let newCmp = observe l r L cmp
      in ActResult
        (st : history)
        (findNextCmp (l : acc) [r] [f] fs (succCnt dsp) mem newCmp b)
-- Break on out-of-order elems.
processAct history st@(MergeState acc (l:|[]) (r:|[]) rest dsp mem cmp b) (Choose R)
    = let newCmp = observe l r R cmp
      in ActResult
        (st : history)
        (findNextCmp (l : acc) [] [] ((r:|[]) : rest) (succCnt dsp) mem newCmp b)

processAct history st@(MergeState acc (l:|ls) rs@(r:|_) rest dsp mem cmp b) (Choose L)
    = let newCmp = observe l r L cmp
      in ActResult
        (st : history)
        (findNextCmp (l : acc) ls (toList rs) rest (succCnt dsp) mem newCmp b)
processAct history st@(MergeState acc ls@(l:|_) (r:|rs) rest dsp mem cmp b) (Choose R)
    = let newCmp = observe l r R cmp
      in ActResult
        (st : history)
        (findNextCmp (r : acc) (toList ls) rs rest (succCnt dsp) mem newCmp b)

processAct history st@(MergeState acc (l:|ls) rs rest dsp mem cmp b) (Boring L)
    = ActResult
        (st : history)
        (findNextCmp acc ls (toList rs) rest (predElem dsp) mem cmp (l:b))
processAct history st@(MergeState acc ls (r:|rs) rest dsp mem cmp b) (Boring R)
    = ActResult
        (st : history)
        (findNextCmp acc (toList ls) rs rest (predElem dsp) mem cmp (r:b))

processAct history st Nop = ActResult (st : history) (Right st)

-- | Find the next state that needs a comparison, or abort with the final
-- list.
findNextCmp
    :: [Int] -- ^ accumulator for current merge (reverse order)
    -> [Int] -- ^ left merge workspace
    -> [Int] -- ^ right merge workspace
    -> [NonEmpty Int] -- ^ lists that have yet to be merged
    -> DisplayState -- ^ the new displayState to use
    -> ElementMap a
    -> Compared Int Choice -- ^ precmp map
    -> [Int] -- ^ boring items
    -> Either [a] (MergeState a)
findNextCmp w x y z d mem cmp b = fix f w x y z
    where
    -- start populating workspace
    f nxt [] [] [] (q:rest) = nxt [] (toList q) [] rest
    -- finish populating workspace
    f _ [] (l:ls) [] (q:rest) = Right (MergeState [] (l:|ls) q rest d mem cmp b)
    -- the final merge
    f _ acc [] [] [] = Left (fmap (flip element mem) (reverse (b ++ acc)))
    -- clean out remaining in left
    f nxt acc l@(_:_) [] rest = nxt (reverse l ++ acc) [] [] rest
    -- clean out remaining in right
    f nxt acc [] r@(_:_) rest = nxt (reverse r ++ acc) [] [] rest
    f nxt acc (l:ls) (r:rs) rest =
        -- Check for PreCmps
        case recompare l r cmp of
            Just L -> nxt (l : acc) ls (r:rs) rest
            Just R -> nxt (r : acc) (l:ls) rs rest
            -- lo, an actual merge
            Nothing -> Right (MergeState acc (l:|ls) (r:|rs) rest d mem cmp b)
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
usort' fn getAct = fix f . ActResult [] . firstCmp
    where
    f _ (ActResult _ (Left final)) = pure final
    f nxt (ActResult h (Right state))
        = getAct (fn state) >>= (nxt . processAct h state)

firstCmp :: Ord a => [a] -> Either [a] (MergeState a)
firstCmp xs =
    let (keys, mem) = elementMap xs
    in findNextCmp [] [] [] (map (:|[]) keys) (DisplayState 0 (length xs)) mem mempty []

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

-- | Compares with '(<=)'
realCompare :: (Applicative f, Ord a) => MergeState a -> f (Action a)
realCompare (MergeState _ (l:|_) (r:|_) _ _ mem _ _)
    = pure $ Choose $ if element l mem <= element r mem then L else R
