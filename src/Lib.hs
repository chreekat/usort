{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Lib where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Operational
import Control.Monad.State
import Data.Foldable
import Data.Maybe (fromJust, catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import Formatting
import System.Console.Haskeline
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Debug.Trace

-- | Given a list of items, use merge sort where the sort function is YOU
-- :D
uSort :: [Text] -> IO [Text]
uSort ts = toList . fromSuccess <$> sortFunc userCompare ts

data Merge a = PickLeft a
             | PickRight a
             | SkipLeft [a]
             | SkipRight [a]
             | BaseCase a
             deriving Show

data SortTree a
    = Noop
    | Single a
    | STree
        { sortL :: SortTree a
        , sortR :: SortTree a
        , merges :: [Merge a]
        }
    deriving Show

instance Foldable SortTree where
    foldMap f = \case
        Noop -> mempty
        Single a -> f a
        STree _ _ ms -> mconcat . map f . concatMap extract $ ms
      where
        extract = \case
            BaseCase a -> [a]
            PickLeft a -> [a]
            PickRight a -> [a]
            SkipLeft as -> as
            SkipRight as -> as

data Sort a = SortSuccess { fromSuccess :: SortTree a }
            | SortFail [a]

sortFunc :: (MonadIO m)
         => User m [Text]
         -> [Text]
         -> m (Sort Text)
sortFunc u xs = traceShow xs $ case xs of
    []  -> pure (SortSuccess Noop)
    [x] -> pure (SortSuccess (Single x))
    xs  -> do
        let (left, right) = splitAt (div (length xs) 2) xs
        SortSuccess actsL <- sortFunc u left
        continueWithL actsL right
  where
    continueWithL actsL = continueWithR actsL <=< sortFunc u
    continueWithR actsL = \case
        SortFail right -> do
            SortSuccess actsL' <- resort u actsL
            continueWithL actsL' right
        SortSuccess actsR -> do
            maybeSorted <-
                merge u (toList actsL) (toList actsR) []
            case maybeSorted of
                Nothing -> continueWithR actsL =<< resort u actsR
                Just acts ->
                    pure (SortSuccess (STree actsL actsR acts))

resort = undefined

merge :: (MonadIO m)
      => User m [Text]
      -> [Text]
      -> [Text]
      -> [Merge Text]
      -> m (Maybe [Merge Text])
merge u xs ys initialActs = traceShow (xs, ys, initialActs) $ case (xs, ys) of
    ([], []) -> pure (Just initialActs)
    ([], ys') -> pure (Just $ SkipRight ys':initialActs)
    (xs', []) -> pure (Just $ SkipLeft xs':initialActs)
    (x:xs', y:ys') -> eval (x,xs') (y,ys') =<< viewT u
  where
    eval :: (MonadIO m)
         => (Text,[Text])
         -> (Text,[Text])
         -> ProgramViewT UserI m [Text]
         -> m (Maybe [Merge Text])
    eval (x,xs) (y,ys) = \case
        Return n -> return (Just initialActs)
        GetNextStep :>>= k -> merge (k (88, 66, x, y)) origXs origYs initialActs
        Compare o :>>= k -> case o of
            LT -> merge (k ()) xs origYs (PickLeft x:initialActs)
            _  -> merge (k ()) origXs ys (PickRight y:initialActs)
        Rewrite1 x' :>>= k -> merge (k ()) (x':xs) origYs initialActs
        Rewrite2 y' :>>= k -> merge (k ()) origXs (y':ys) initialActs
        Undo :>>= k ->
            case initialActs of
                [] -> return Nothing
                as ->
                    let (as', undoneL, undoneR) = undo as origXs origYs
                    in merge (k ()) undoneL undoneR as'
      where
        origXs = x:xs
        origYs = y:ys

undo :: [Merge a] -> [a] -> [a] -> ([Merge a], [a], [a])
undo as left right = case as of
    [] -> (as, left, right)
    (a:as) -> case a of
        -- Just redo
        BaseCase _ -> (as, left, right)
        PickLeft b -> (as, b:left, right)
        PickRight b -> (as, left, b:right)
        SkipLeft bs -> undo as (bs++left) right
        SkipRight bs -> undo as left (bs++right)

-- ##
-- ## operations we permit the user
-- ##

data UserI a where
    GetNextStep :: UserI (Int, Int, Text, Text)
    Compare     :: Ordering -> UserI ()
    Rewrite1    :: Text -> UserI ()
    Rewrite2    :: Text -> UserI ()
    Undo        :: UserI ()

type User m a = ProgramT UserI m a

getNextStep = singleton GetNextStep

-- Here's a "program".
userCompare :: MonadIO m => User m [Text]
userCompare = forever (getNextStep >>= doSomething)
  where
    doSomething :: MonadIO m => (Int, Int, Text, Text) -> User m ()
    doSomething v@(rem, est, x, y) = do
        liftIO (printPrompt (rem, est, x, y))
        c <- liftIO getResponse
        case c of
            '1' -> singleton (Compare LT)
            '2' -> singleton (Compare GT)
            'e' -> singleton . either Rewrite1 Rewrite2 =<< liftIO (editItem x y)
            'u' -> singleton Undo
            _   -> unknownCommand (doSomething v)

getResponse = getChar <* putStrLn ""

unknownCommand cont = do
    liftIO $ putStrLn "Unknown command. Let's try again."
    cont

editItem :: Text -> Text -> IO (Either Text Text)
editItem x y = do
    putStr "Which item? > "
    c <- getResponse
    case c of
        '1' -> Left  <$> replaceText x
        '2' -> Right <$> replaceText y
        _   -> unknownCommand (editItem x y)

replaceText t = do
    replacement <-
        runInputT defaultSettings
                  (getInputLineWithInitial "What instead? > " ("", T.unpack t))
    return (maybe t T.pack replacement)

printPrompt :: (Int, Int, Text, Text) -> IO ()
printPrompt (remaining, estimate, x, y) = do
    T.putStrLn (sformat hdrFmt remaining estimate)
    T.putStrLn ("-- (1) " <> x)
    T.putStrLn ("-- (2) " <> y)
    T.putStrLn "-- Or [e]dit an entry"
    T.putStrLn "-- Or [u]ndo last comparison"
    T.putStr "-> "
  where
    hdrFmt = "(~" % int % "/" % int % ") Which is more important?"
