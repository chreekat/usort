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

data Merge a = PickLeft a
             | PickRight a
             | SkipLeft (NonEmpty a)
             | SkipRight (NonEmpty a)
             deriving Show

instance ToNonEmpty Merge where
    toList' = \case
        PickLeft a -> a :| []
        PickRight a -> a :| []
        SkipLeft ne -> ne
        SkipRight ne -> ne

instance Foldable Merge where
    foldMap f = \case
        PickLeft a -> f a
        PickRight a -> f a
        SkipLeft (a :| as) -> mconcat (f a : map f as)
        SkipRight (a :| as) -> mconcat (f a : map f as)

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
        Single a -> a :| []
        STree _ _ ms -> sconcat (fmap toList' ms)

instance Foldable SortTree where
    foldMap f = \case
        Single a -> f a
        STree _ _ ms -> foldMap f (concatMap toList ms)

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
        mf <- merge u (toList' actsL) (toList' actsR)
        case mf of
            -- Can't merge? Try the right half again.
            Nothing -> tryRight u actsL =<< resort u actsR
            Just f -> pure (Right (STree actsL actsR f))

-- | Resort takes a tree and pops enough actions so that the user has
-- something to do, then continues. Failing that, it returns the list the
-- tree represents.

resort :: MonadIO m
       => User m [Text]
       -> SortTree Text
       -> m (Either (NonEmpty Text) (SortTree Text))
resort u (Single a) = pure (Left (a :| []))
resort u (STree actsL actsR m) =
    fmap (STree actsL actsR) <$> resort' u [] [] m

resort' :: MonadIO m
        => User m [Text]
        -> [Text]
        -> [Text]
        -> NonEmpty (Merge Text)
        -> m (Either (NonEmpty Text) (NonEmpty (Merge Text)))
resort' u lefts rights (a :| as) = case a of
        -- Fill the right a bit. Can't pop more, so this is the end.
        SkipRight xs@(x :| xs') -> case lefts of
            [] -> pure (Left (x :| xs' <> rights)) -- Nothing to merge.
            (l:ls) -> do
                -- Merge our right-skipped xs, plus any other rights
                mm <- merge u (l :| ls) (x :| xs' <> rights)
                case mm of
                    -- Unsuccessful merge. Bail with lefts + rights
                    Nothing -> pure (Left (l :| ls <> toList xs <> rights))
                    -- Success!
                    Just m -> pure (Right m)
        -- Same as above
        SkipLeft xs@(x :| xs') -> case rights of
            [] -> pure (Left (x :| xs' <> lefts)) -- Nothing to merge.
            (r:rs) -> do
                -- Merge our right-skipped xs, plus any other rights
                mm <- merge u (x :| xs' <> lefts) (r :| rs)
                case mm of
                    -- Unsuccessful merge. Bail with lefts + rights
                    Nothing -> pure (Left (x :| xs' <> lefts <> rights))
                    -- Success!
                    Just m -> pure (Right m)
        PickLeft x -> case rights of
            [] -> case as of
                [] -> pure (Left (x :| []))
                (a':as') -> resort' u [x] [] (a' :| as')
            (r:rs) -> do
                mm <- merge u (x :| lefts) (r :| rs)
                case mm of
                    Nothing -> case as of
                        [] -> pure (Left (x :| lefts <> (r : rs)))
                        (a':as') -> resort' u (x : lefts) rights (a' :| as')
                    Just m -> pure (Right m)
        -- Same as above
        PickRight x -> case lefts of
            [] -> case as of
                [] -> pure (Left (x :| []))
                (a':as') -> resort' u [] [x] (a' :| as')
            (l:ls) -> do
                mm <- merge u (l :| ls) (x :| rights)
                case mm of
                    Nothing -> case as of
                        [] -> pure (Left ( l :| lefts <> (x : rights)))
                        (a':as') -> resort' u lefts (x : rights) (a' :| as')
                    Just m -> pure (Right m)

merge :: (MonadIO m)
      => User m [Text]
      -> NonEmpty Text
      -> NonEmpty Text
      -> m (Maybe (NonEmpty (Merge Text)))
merge u l r = merge' u l r Nothing

merge' = undefined
-- merge' u xs ys f = eval xs ys =<< viewT u
--   where
--     eval :: (MonadIO m)
--          => NonEmpty Text
--          -> NonEmpty Text
--          -> ProgramViewT UserI m [Text]
--          -> m [Merge Text]
--     eval (x :| xs) (y :| ys) = \case
--         Return n -> pure Nothing
--         GetNextStep :>>= k -> merge (k (88, 66, x, y)) origXs origYs initialActs
--         Compare o :>>= k -> case o of
--             LT -> merge (k ()) xs origYs (PickLeft x . f)
--             _  -> merge (k ()) origXs ys (PickRight y . f)
--         Rewrite1 x' :>>= k -> merge (k ()) (x':xs) origYs f
--         Rewrite2 y' :>>= k -> merge (k ()) origXs (y':ys) f
--         Undo :>>= k ->
--             case (f ()) of
--                 [] -> return []
--                 (a:as) -> case undo (a :| as) origXs origYs of
--                     Nothing -> pure []
--                     Just (as', undoneL, undoneR) ->
--                         merge (k ()) undoneL undoneR as'

undo :: NonEmpty (Merge a) -> [a] -> [a] -> Maybe ([Merge a], NonEmpty a, NonEmpty a)
undo (a :| as) left right = case a of
    PickLeft x -> case right of
        [] -> case as of
            [] -> Nothing
            (a':as') -> undo (a':|as') (x:left) right
        (r:rs)  -> Just (as, x :| left, r :| rs)
    PickRight x -> case left of
        [] -> case as of
            [] -> Nothing
            (a':as') -> undo (a':|as') (x:left) right
        (r:rs)  -> Just (as, x :| left, r :| rs)
    SkipLeft xs -> case right of
        [] -> Nothing
        (r:rs) -> Just (as, xs, r :| rs)
    SkipRight xs -> case left of
        [] -> Nothing
        (l:ls) -> Just (as, l :| ls, xs)

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

