{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Lib where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Operational
import Control.Monad.State
import Data.Maybe (fromJust)
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
uSort ts = evalStateT (sortFunc userCompare ts) []

data Undo a = URewrite1 Text
            | URewrite2 Text
            | UPickLeft a
            | UPickRight a

sortFunc :: (MonadIO m, MonadState s m, s ~ [Undo Text])
         => User m [Text]
         -> [Text]
         -> m [Text]
sortFunc u = \case
    []  -> pure []
    [x] -> pure [x]
    xs  -> do
        let (h1, h2) = splitAt (div (length xs) 2) xs
        l1 <- sortFunc u h1
        l2 <- sortFunc u h2
        merge u l1 l2

merge :: (MonadIO m, MonadState s m, s ~ [Undo Text])
      => User m [Text]
      -> [Text]
      -> [Text]
      -> m [Text]
merge u xs ys = case (xs, ys) of
    ([], []) -> pure []
    ([], ys') -> pure ys'
    (xs', []) -> pure xs'
    (x:xs', y:ys') -> eval (x,xs') (y,ys') =<< viewT u
  where
    eval :: (MonadIO m, MonadState s m, s ~ [Undo Text])
         => (Text,[Text])
         -> (Text,[Text])
         -> ProgramViewT UserI m [Text]
         -> m [Text]
    eval (x,xs) (y,ys) = \case
        Return n -> return n
        GetNextStep :>>= k -> merge (k (88, 66, x, y)) origXs origYs
        Compare o :>>= k -> case o of
            LT -> (:) <$> pure x <*> merge (k ()) xs origYs
            _  -> (:) <$> pure y <*> merge (k ()) origXs ys
        Rewrite1 x' :>>= k -> merge (k ()) (x':xs) origYs
        Rewrite2 y' :>>= k -> merge (k ()) origXs (y':ys)
        -- Just repeat for now
        Undo :>>= k ->
            liftIO (putStrLn "Lol nope") >> merge (k ()) origXs origYs
      where
        origXs = x:xs
        origYs = y:ys

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
