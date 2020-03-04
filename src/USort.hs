{-|
 - usort!
 - 
 - In order to undo, all previous program states will be pushed to a stack. O(n *
 - (n lg n)) storage, but if n is large enough to matter when *you* are the sort
 - function, you have bigger problems.
 - 
 - I'll use an iterative merge sort so all state is easy to get at and think about.
 -
 -}

{-# LANGUAGE DeriveGeneric #-}
module USort (module Types, module USort) where

import Control.Monad.Fix
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import GHC.Generics
import qualified Data.List.NonEmpty as NE

import Types

-- | Decisions, decisions.
data Choice = L | R
    deriving (Eq, Show)

-- | Merge actions (things a user may do)
data Action = Choose Choice | Delete Choice | Edit Choice Text | Undo
    deriving (Eq, Show, Generic)

-- | Holds the new history and the next merge state.
newtype ActResult = ActResult { unResult :: ([MergeState], Either [Text] MergeState) }
    deriving (Eq, Show)

-- | Process given action given a history and current state.
processAct
    :: [MergeState] -- ^ history
    -> MergeState -- ^ current
    -> Action -- ^ to be processed
    -> ActResult

processAct history st@(MergeState acc (_:|ls) (r:|rs) rest) (Delete L) =
    ActResult (st : history, findNextMerge acc ls (r:rs) rest)
processAct history st@(MergeState acc (l:|ls) (_:|rs) rest) (Delete R) = 
    ActResult (st : history, findNextMerge acc (l:ls) rs rest)

processAct history st@(MergeState acc (_:|ls) (r:|rs) rest) (Edit L new) = 
    ActResult (st : history, Right $ MergeState acc (new:|ls) (r:|rs) rest)
processAct history st@(MergeState acc (l:|ls) (_:|rs) rest) (Edit R new) = 
    ActResult (st : history, Right $ MergeState acc (l:|ls) (new:|rs) rest)

processAct [] state Undo = ActResult ([], Right state)
processAct (s:ss) _ Undo = ActResult (ss, Right s)

-- Assume mostly sorted input during initial pass. It has all one-item NonEmpty lists,
-- which is how we distinguish it from other cases.
processAct history st@(MergeState acc (l:|[]) (r:|[]) ((q:|[]):qss)) (Choose L)
    = ActResult
        (st : history, Right $ MergeState (l : acc) (r :| []) (q :| []) qss)
-- Special case: at the end of the input of a totally sorted list!
processAct history st@(MergeState acc (l:|[]) (r:|[]) []) (Choose L) =
    ActResult (st : history, Left $ reverse (r : l : acc))
processAct history st@(MergeState acc (l:|ls) (r:|rs) rest) (Choose L) =
    ActResult (st : history, findNextMerge (l : acc) ls (r : rs) rest)
-- Break up runs of sorted subsections during initial pass. This "wastes"
-- one compare by using it to break up the run rather than create a real
-- merge.
processAct history st@(MergeState acc (l:|[]) (r:|[]) rest) (Choose R) =
    ActResult (st : history, findNextMerge (l : acc) [] [] ((r :| []) : rest))
processAct history st@(MergeState acc (l:|ls) (r:|rs) rest) (Choose R) =
    ActResult (st : history, findNextMerge (r : acc) (l : ls) rs rest)

-- | Find the next state that needs a merge action, or abort with the final
-- list.
findNextMerge
    :: [Text] -- ^ accumulator for current merge
    -> [Text] -- ^ left merge workspace
    -> [Text] -- ^ right merge workspace
    -> [NonEmpty Text] -- ^ lists that have yet to be merged
    -> Either [Text] MergeState
findNextMerge w x y z = fix f w x y z
    where
    -- start populating workspace
    f nxt [] [] [] (q:rest) = nxt [] (toList q) [] rest
    -- finish populating workspace
    f _ [] (l:ls) [] (q:rest) = Right (MergeState [] (l:|ls) q rest)
    -- the final merge
    f _ acc [] [] [] = Left (reverse acc)
    -- clean out remaining in left
    f nxt acc l@(_:_) [] rest = nxt (reverse l ++ acc) [] [] rest
    -- clean out remaining in right
    f nxt acc [] r@(_:_) rest = nxt (reverse r ++ acc) [] [] rest
    -- lo, an actual merge
    f _ acc (l:ls) (r:rs) rest = Right (MergeState acc (l:|ls) (r:|rs) rest)
    -- finalize last merge
    f nxt (a:as) [] [] rest = nxt [] [] [] (rest ++ [NE.reverse (a:|as)])

-- | Sorts the input, given an action that produces 'Action's!
usort :: Monad m
    => (MergeState -> m Action) -- ^ Produces an Action
    -> [Text] -- ^ Input list
    -> m [Text]
usort getAct xs =
    fix f (ActResult ([], findNextMerge [] [] [] (map (:|[]) xs)))
    where
    f _ (ActResult (_, Left final)) = pure final
    f nxt (ActResult (h, Right state)) = getAct state >>= (nxt . processAct h state)

realCompare :: Applicative f => MergeState -> f Action
realCompare (MergeState _ (l:|_) (r:|_) _) = pure $ Choose $ if l <= r then L else R
