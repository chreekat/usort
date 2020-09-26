{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.List.NonEmpty (NonEmpty (..))

import SplitItems
import USort

import Miso
import Miso.String (toMisoString, fromMisoString, ms, MisoString)
import qualified Data.Text as T

-- jsaddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif


data AppView = Input | Confirm | Compare | Done
    deriving (Eq, Show)

data TopModel' a = TopModel
    { initialInput :: a
    , initialLines :: [a]
    , appView :: AppView
    , cmpHistory :: [MergeState a]
    , cmpMergeState :: Maybe (MergeState a)
    , cmpEditBuf :: Maybe a
    , cmpResult :: [a]
    } deriving (Eq, Show)

type TopModel = TopModel' MisoString

initModel :: TopModel
initModel = TopModel "" [] Input [] Nothing Nothing []

data UIAction' a
    = Nope
    | UpdateInput MisoString
    | ConfirmInput
    | Back
    | StartCompare
    | CompareAct (Action a)
    | Finish

type UIAction = UIAction' MisoString

update' :: UIAction -> TopModel -> Effect UIAction TopModel
update' a m = case a of
    Nope -> noEff m
    UpdateInput s -> noEff m { initialInput = s }
    ConfirmInput -> noEff m
        { initialLines =
            let
                split' = items . splitItems . T.lines
                thoseLines
                    = map toMisoString
                    . split'
                    . fromMisoString
                    . initialInput
            in thoseLines m
        , appView = Confirm
        }
    Back -> noEff m { appView = Input }
    StartCompare ->
        case firstCmp (initialLines m) of
            Left xs -> error "noEff (ModelResult xs) >> pure Finish"
            Right ms -> noEff m
                { appView = Compare
                , cmpMergeState = Just ms
                }

    -- In usort-console, we build our own UI loop. But on the web, we just hook
    -- into the browser's loop. A user choice shows up as an 'Action'; we
    -- 'processAct' on it, and use 'ActResult' to update the model. That's, uh,
    -- all there is to it. Our model is nearly just the ActResult itself, except
    -- that we need to eject up a level in the `Left` case. Oh, we also need
    -- some scratch space for "modals" (edits, deletes).
    CompareAct act ->
        maybe
            (error "Got a CompareAct when no comparison is in progress.")
            (\(ActResult newHist res) ->
                case res of
                    Left res -> noEff m
                        { cmpHistory = newHist
                        , cmpResult = res
                        , appView = Done
                        }
                    Right newMs -> noEff m
                        { cmpHistory = newHist
                        , cmpMergeState = Just newMs
                        })
            ((processAct (cmpHistory m) `flip` act) <$> cmpMergeState m)

topView :: TopModel -> View UIAction
topView m =
    div_ []
        [ h1_ [] [text "U Sort It"]
        , div_ [] [rest]
        ]
  where
    rest = case (appView m) of
        Input -> inputView
        Confirm -> confirmView
        Compare -> cmpView
        Done -> ul_ [] (map (li_ [] . (:[]) . text . ms) (cmpResult m))
    inputView =
        div_ []
            [ textarea_
                [ wrap_ "off", cols_ "80", rows_ "50"
                , onInput UpdateInput
                ]
                [ text (ms (initialInput m)) ]
            , button_ [ onClick ConfirmInput ] ["Click"]
            ]
    confirmView =
        div_ []
            [ div_ [] ["You want me to sort this, yeah?"]
            , ul_ [] (fmap (li_ [] . (:[]) . pre_ [] . (:[]) . text) (initialLines m))
            , button_ [ onClick Back ] ["No"]
            , button_ [ onClick StartCompare ] ["Yes"]
            ]
    cmpView =
        let Just (MergeState _ (l :| _)  (r :| _) _ d _) = cmpMergeState m
        in ul_ [] (
            [ div_ [] ["Which is more important?"]
            , li_ [] [text (ms l)]
            , li_ [] [text (ms r)]
            ]
            <> map (\(a, t) -> button_ [ onClick a ] [t])
                [ (CompareAct (Choose L), "Left")
                , (CompareAct (Choose R), "Right")
                ]
        )


#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
  Warp.runSettings (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
    JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

main :: IO ()
main = runApp $ startApp App {..} where
    initialAction = Nope
    model = initModel
    update = update'
    view = topView
    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off
