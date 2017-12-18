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

module USort (module Types, module USort) where

import Control.Monad.Fix
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.List.NonEmpty as NE

import Types

data Choice = L | R
    deriving (Eq, Show)
data Action = Choose Choice | Delete Choice | Edit Choice Text | Undo
    deriving (Eq, Show)

canUndo :: Action -> Bool
canUndo Undo = False
canUndo _ = True

newtype ActResult = ActResult { unResult :: ([State], Either [Text] State) }
    deriving (Eq, Show)

processAct :: [State] -> State -> Action -> ActResult

processAct history st@(State acc (_:|ls) (r:|rs) rest) (Delete L) =
    ActResult (st : history, findNextMerge acc ls (r:rs) rest)
processAct history st@(State acc (l:|ls) (_:|rs) rest) (Delete R) = 
    ActResult (st : history, findNextMerge acc (l:ls) rs rest)

processAct history st@(State acc (_:|ls) (r:|rs) rest) (Edit L new) = 
    ActResult (st : history, Right $ State acc (new:|ls) (r:|rs) rest)
processAct history st@(State acc (l:|ls) (_:|rs) rest) (Edit R new) = 
    ActResult (st : history, Right $ State acc (l:|ls) (new:|rs) rest)

processAct [] state Undo = ActResult ([], Right state)
processAct (s:ss) _ Undo = ActResult (ss, Right s)

processAct history st@(State acc (l:|ls) (r:|rs) rest) (Choose L) =
    ActResult (st : history, findNextMerge (l:acc) ls (r:rs) rest)
processAct history st@(State acc (l:|ls) (r:|rs) rest) (Choose R) =
    ActResult (st : history, findNextMerge (r:acc) (l:ls) rs rest)

findNextMerge :: [Text] -> [Text] -> [Text] -> [NonEmpty Text] -> Either [Text] State
findNextMerge w x y z = fix f w x y z
    where
    -- start populating workspace
    f nxt [] [] [] (q:rest) = nxt [] (toList q) [] rest
    -- finish populating workspace
    f _ [] (l:ls) [] (q:rest) = Right (State [] (l:|ls) q rest)
    -- the final merge
    f _ acc [] [] [] = Left (reverse acc)
    -- clean out remaining in left
    f nxt acc l@(_:_) [] rest = nxt (reverse l ++ acc) [] [] rest
    -- clean out remaining in right
    f nxt acc [] r@(_:_) rest = nxt (reverse r ++ acc) [] [] rest
    -- lo, an actual merge
    f _ acc (l:ls) (r:rs) rest = Right (State acc (l:|ls) (r:|rs) rest)
    -- finalize last merge
    f nxt (a:as) [] [] rest = nxt [] [] [] (rest ++ [NE.reverse (a:|as)])

usort :: MonadFix m => (State -> m Action) -> [Text] -> m [Text]
usort getAct xs =
    fix f (ActResult ([], findNextMerge [] [] [] (map (:|[]) xs)))
    where
    f _ (ActResult (_, Left final)) = pure final
    f nxt (ActResult (h, Right state)) = getAct state >>= (nxt . processAct h state)

realSort :: Applicative f => State -> f Action
realSort (State _ (l:|_) (r:|_) _) = pure $ Choose $ if l <= r then L else R
