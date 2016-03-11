{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

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

-- | Given a list of items, use merge sort where the sort function is YOU
-- :D
uSort :: [Text] -> IO [Text]
uSort [] = pure []
uSort (x:xs) = toList . fromSuccess <$> sortFunc userCompare (NE x xs)

class ToNonEmpty m where
    toList' :: m a -> NonEmpty a

data NonEmpty a = NE a [a]
    deriving Show

instance ToNonEmpty NonEmpty where
    toList' = id

instance Foldable NonEmpty where
    foldMap f (NE x xs) = f x <> mconcat (map f xs)

-- | Does a weird even/odd thing, but whatevs
half x x' = \case
    [] -> (NE x [], NE x' [])
    [x''] -> (NE x [], NE x' [x''])
    (z:z':zs) -> let (h,h') = half z z' zs
                 in (NE x (toList h), NE x' (toList h'))

reverse' (NE x xs) = case reverse xs of
    [] -> NE x xs
    (x':xs') -> NE x' (xs' ++ [x])

data Merge a = PickLeft a
             | PickRight a
             | SkipLeft (NonEmpty a)
             | SkipRight (NonEmpty a)
             deriving Show

instance ToNonEmpty Merge where
    toList' = \case
        PickLeft a -> NE a []
        PickRight a -> NE a []
        SkipLeft ne -> ne
        SkipRight ne -> ne

instance Foldable Merge where
    foldMap f = \case
        PickLeft a -> f a
        PickRight a -> f a
        SkipLeft (NE a as) -> mconcat (f a : map f as)
        SkipRight (NE a as) -> mconcat (f a : map f as)

data SortTree a
    = Single a
    | STree
        { sortL :: SortTree a
        , sortR :: SortTree a
        , merges :: NonEmpty (Merge a)
        }
    deriving Show

instance ToNonEmpty SortTree where
    toList' = \case
        Single a -> NE a []
        STree _ _ (NE m ms) ->
            let (NE x xs) = toList' m
                xs' = concatMap toList ms
            in NE x (xs ++ xs')

instance Foldable SortTree where
    foldMap f = \case
        Single a -> f a
        STree _ _ ms -> mconcat . map f . concatMap extract . reverse' $ ms
      where
        extract = \case
            PickLeft a -> [a]
            PickRight a -> [a]
            SkipLeft (NE a as) -> a:as
            SkipRight (NE a as) -> a:as

data Sort a = SortSuccess { fromSuccess :: SortTree a }
            | SortFail (NonEmpty a)

sortFunc :: (MonadIO m)
         => User m [Text]
         -> NonEmpty Text
         -> m (Sort Text)
sortFunc u (NE x xs) = case xs of
    [] -> pure (SortSuccess (Single x))
    (x':xs')  -> do
        let (left, right) = half x x' xs'
        SortSuccess actsL <- sortFunc u left
        continueWithL u actsL right

continueWithL :: MonadIO m
              => User m [Text]
              -> SortTree Text
              -> NonEmpty Text
              -> m (Sort Text)
continueWithL u actsL = continueWithR u actsL <=< sortFunc u

continueWithR :: MonadIO m
              => User m [Text]
              -> SortTree Text
              -> Sort Text
              -> m (Sort Text)
continueWithR u actsL = \case
    SortFail right -> do
        SortSuccess actsL' <- resort u actsL
        continueWithL u actsL' right
    SortSuccess actsR -> do
        maybeActs <-
            merge u (toList actsL) (toList actsR) []
        case maybeActs of
            [] -> continueWithR u actsL =<< resort u actsR
            (a:as) ->
                pure (SortSuccess (STree actsL actsR (NE a as)))

resort :: MonadIO m => User m [Text] -> SortTree Text -> m (Sort Text)
resort u = \case
    Single a -> pure (SortFail (NE a []))
    tree@STree{..} -> case undo merges [] [] of
        Nothing -> pure (SortFail (toList' tree))
        Just (as', undoneL, undoneR) -> do
            maybeActs <- merge u undoneL undoneR as'
            case maybeActs of
                [] -> continueWithR u sortL =<< resort u sortR
                (a:as) ->
                    pure (SortSuccess (tree { merges = NE a as } ))

merge :: (MonadIO m)
      => User m [Text]
      -> [Text]
      -> [Text]
      -> [Merge Text]
      -> m [Merge Text]
merge u xs ys initialActs = case (xs, ys) of
    ([], []) -> pure initialActs
    ([], y:ys') -> pure (SkipRight (NE y ys'):initialActs)
    (x:xs', []) -> pure (SkipLeft (NE x xs'):initialActs)
    (x:xs', y:ys') -> eval (NE x xs') (NE y ys') =<< viewT u
  where
    eval :: (MonadIO m)
         => NonEmpty Text
         -> NonEmpty Text
         -> ProgramViewT UserI m [Text]
         -> m [Merge Text]
    eval (NE x xs) (NE y ys) = \case
        Return n -> pure initialActs
        GetNextStep :>>= k -> merge (k (88, 66, x, y)) origXs origYs initialActs
        Compare o :>>= k -> case o of
            LT -> merge (k ()) xs origYs (PickLeft x:initialActs)
            _  -> merge (k ()) origXs ys (PickRight y:initialActs)
        Rewrite1 x' :>>= k -> merge (k ()) (x':xs) origYs initialActs
        Rewrite2 y' :>>= k -> merge (k ()) origXs (y':ys) initialActs
        Undo :>>= k ->
            case initialActs of
                [] -> return []
                (a:as) -> case undo (NE a as) origXs origYs of
                    Nothing -> pure []
                    Just (as', undoneL, undoneR) ->
                        merge (k ()) undoneL undoneR as'
      where
        origXs = x:xs
        origYs = y:ys
        push as a = Just (NE a []) `mplus` as

undo :: NonEmpty (Merge a) -> [a] -> [a] -> Maybe ([Merge a], [a], [a])
undo (NE a as) left right = case a of
    PickLeft x -> Just (as, x:left, right)
    PickRight x -> Just (as, left, x:right)
    SkipLeft xs -> case as of
        [] -> Nothing
        (a':as') -> undo (NE a' as') (toList xs++left) right
    SkipRight xs -> case as of
        [] -> Nothing
        (a':as') -> undo (NE a' as') left (toList xs++right)

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
