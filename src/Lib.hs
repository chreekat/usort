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
import Data.Semigroup
import Data.List.NonEmpty hiding (toList, map, reverse)
import Data.Text (Text)
import Formatting
import System.Console.Haskeline
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List.NonEmpty as NE

-- | Given a list of items, use merge sort where the sort function is YOU
-- :D
uSort :: [Text] -> IO [Text]
uSort [] = pure []
uSort (x:xs) = toList . fromRight <$> sortFunc userCompare (x :| xs)

fromRight (Right x) = x

class ToNonEmpty m where
    toList' :: m a -> NonEmpty a

instance ToNonEmpty NonEmpty where
    toList' = id

-- | Does a weird even/odd thing, but whatevs
half x x' = \case
    [] -> (x :| [], x' :| [])
    [x''] -> (x :| [], x' :| [x''])
    (z:z':zs) -> let (h,h') = half z z' zs
                 in (x <| h, x' <| h')

data Merge a = PickLeft a (Merge a)
             | PickRight a (Merge a)
             | SkipLeft (NonEmpty a)
             | SkipRight (NonEmpty a)
             deriving Show

instance ToNonEmpty Merge where
    toList' = \case
        PickLeft a c -> a <| toList' c
        PickRight a c -> a <| toList' c
        SkipLeft ne -> ne
        SkipRight ne -> ne

instance Foldable Merge where
    foldMap f = \case
        PickLeft a c -> f a `mappend` foldMap f c
        PickRight a c -> f a `mappend` foldMap f c
        SkipLeft (a :| as) -> mconcat (f a : map f as)
        SkipRight (a :| as) -> mconcat (f a : map f as)

data SortTree a
    = Single a
    | STree
        { sortL :: SortTree a
        , sortR :: SortTree a
        , merges :: Merge a
        }
    deriving Show

instance ToNonEmpty SortTree where
    toList' = \case
        Single a -> a :| []
        STree _ _ ms -> case ms of
            PickLeft a c -> a <| toList' c
            PickRight a c -> a <| toList' c
            SkipLeft as -> as
            SkipRight as -> as

instance Foldable SortTree where
    foldMap f = \case
        Single a -> f a
        STree _ _ ms -> foldMap f ms

sortFunc :: (MonadIO m)
         => User m [Text]
         -> NonEmpty Text
         -> m (Either (NonEmpty Text) (SortTree Text))
sortFunc u (x :| xs) = case xs of
    [] -> pure (Right (Single x))
    (x':xs')  -> do
        let (left, right) = half x x' xs'
        tryLeft u right =<< sortFunc u left

tryLeft :: MonadIO m
        => User m [Text]
        -> NonEmpty Text
        -> Either (NonEmpty Text) (SortTree Text)
        -> m (Either (NonEmpty Text) (SortTree Text))
tryLeft u right = \case
    Left left -> sortFunc u (left<>right)
    Right actsL -> tryRight u actsL =<< sortFunc u right

tryRight :: MonadIO m
         => User m [Text]
         -> SortTree Text
         -> Either (NonEmpty Text) (SortTree Text)
         -> m (Either (NonEmpty Text) (SortTree Text))
tryRight u actsL = \case
    -- Couldn't sort the right half? Try the left half again.
    Left right -> tryLeft u right =<< resort u actsL
    Right actsR -> do
        mms <- merge u (toList' actsL) (toList' actsR) []
        case mms of
            -- Can't merge? Try the right half again.
            Nothing -> tryRight u actsL =<< resort u actsR
            Just ms -> pure (Right (STree actsL actsR ms))

resort :: MonadIO m => User m [Text] -> SortTree Text -> m (Either (NonEmpty Text) (SortTree Text))
resort u = \case
    Single a -> pure (Left (a :| []))
    tree@STree{..} -> case undo merges [] [] of
        Nothing -> pure (Left (toList' tree))
        Just (as', undoneL, undoneR) -> do
            merges <- merge u undoneL undoneR as'
            continueWithBoth u sortL sortR merges

continueWithBoth :: MonadIO m
                 => User m [Text]
                 -> SortTree Text
                 -> SortTree Text
                 -> [Merge Text]
                 -> m (Either (NonEmpty Text) (SortTree Text))
continueWithBoth u l r = \case
    [] -> continueWithR u l =<< resort u r
    (a:as) ->
        pure (Right (STree l r (a :| as)))

merge :: (MonadIO m)
      => User m [Text]
      -> NonEmpty Text
      -> NonEmpty Text
      -> [Merge Text]
      -> m (Maybe (Merge Text))
merge u xs ys initialActs = case (xs, ys) of
    ([], []) -> pure initialActs
    ([], y:ys') -> pure (SkipRight (y :| ys'):initialActs)
    (x:xs', []) -> pure (SkipLeft (x :| xs'):initialActs)
    (x:xs', y:ys') -> eval (x :| xs') (y :| ys') =<< viewT u
  where
    eval :: (MonadIO m)
         => NonEmpty Text
         -> NonEmpty Text
         -> ProgramViewT UserI m [Text]
         -> m [Merge Text]
    eval (x :| xs) (y :| ys) = \case
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
                (a:as) -> case undo (a :| as) origXs origYs of
                    Nothing -> pure []
                    Just (as', undoneL, undoneR) ->
                        merge (k ()) undoneL undoneR as'
      where
        origXs = x:xs
        origYs = y:ys
        push as a = Just (a :| []) `mplus` as

undo :: Merge a -> [a] -> [a] -> Maybe (Maybe (Merge a), NonEmpty a, NonEmpty a)
undo a left right = case a of
    PickLeft x c -> case right of
        [] -> undo c (x:left) right
        (r:rs)  -> Just (Just c, x :| left, r :| rs)
    PickRight x c -> case left of
        [] -> undo c left (x:right)
        (l:ls)  -> Just (Just c, l :| ls, x :| right)
    SkipLeft xs -> case right of
        [] -> Nothing
        (r:rs) -> Just (Nothing, xs, r:|rs)
    SkipRight xs -> case left of
        [] -> Nothing
        (l:ls) -> Just (Nothing, l:|ls, xs)

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

