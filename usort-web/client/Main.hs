{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List.NonEmpty (NonEmpty (..))

import SplitItems
import USort

import Miso
import Miso.String (toMisoString, fromMisoString, ms, MisoString)

import qualified Data.Text as T

-- * Input app

type InputModel = MisoString

-- inputView :: View UIAction
inputView m wrap =
    div_ []
        [ textarea_
            [ wrap_ "off", cols_ "80", rows_ "50"
            , onInput (wrap . UpdateInput)
            ]
            [ text (ms m) ]
        , button_ [ onClick (wrap BeginSort) ] ["Click"]
        ]

data InputAction = UpdateInput MisoString | BeginSort

inputUpdate a m wrapModel ejectAct = case a of
    UpdateInput j -> noEff (wrapModel j)
    BeginSort -> ejectAct a m

-- * Process app

type ProcessModel = MisoString

-- processView :: MisoString -> View UIAction
processView stuff wrap =
    let is = map toMisoString (items (splitItems (T.lines (fromMisoString stuff))))
    in div_ []
        [ div_ [] ["You want me to sort this, yeah?"]
        , ul_ [] (fmap (li_ [] . (:[]) . pre_ [] . (:[]) . text) is)
        , button_ [ onClick (wrap Back) ] ["No"]
        , button_ [ onClick (wrap (Sort is)) ] ["Yes"]
        ]

data ProcessAction = Back | Sort [MisoString]

-- processUpdate :: ProcessAction -> ProcessModel -> Effect a m
processUpdate a m _wrapModel ejectAct = case a of
    Back -> ejectAct a m
    Sort _ -> ejectAct a m

-- * Cmp app
--
-- In usort-console, we build our own UI loop. But on the web, we just hook into
-- the browser's loop. A user choice shows up as an 'Action'; we 'processAct'
-- on it, and use 'ActResult' to update the model. That's, uh, all there is to
-- it. Our model is nearly just the ActResult itself, except that we need to
-- eject up a level in the `Left` case. Oh, we also need some scratch space for
-- "modals" (edits, deletes).

data CmpModel = CmpModel
    { cmpHistory :: [MergeState MisoString]
    , cmpMergeState :: MergeState MisoString
    , cmpEditBuf :: Maybe MisoString
    -- , modal :: Maybe CmpModal
    } deriving (Eq, Show)

cmpView m wrap =
    let MergeState _ (l :| _)  (r :| _) _ d _ = cmpMergeState m
    in ul_ [] (
        [ div_ [] ["Which is more important?"]
        , li_ [] [text (ms l)]
        , li_ [] [text (ms r)]
        ]
        <> map (\(a, t) -> button_ [ onClick a ] [t])
            [ (wrap (Choose L), "Left")
            , (wrap (Choose R), "Right")
            ]
    )

data CmpAction = Done [MisoString]

processCmp act (CmpModel hist ms x) wrapModel ejectAct =
    let ActResult newHist res = processAct hist ms act
    in case res of
        Left res -> ejectAct (Done res)
        Right newMs -> noEff (wrapModel (CmpModel newHist newMs x))

-- * Result app

-- No findings :<

-- * Top level composition

data TopModel
    = ModelInput InputModel
    | ModelProcess ProcessModel
    | ModelCmp CmpModel
    | ModelResult ResultModel
    deriving (Eq, Show)

type ResultModel = [MisoString]

topView :: TopModel -> View UIAction
topView m =
    div_ []
        [ h1_ [] [text "U Sort It"]
        , div_ [] rest
        ]
  where
    rest =
        case m of
        ModelInput i -> [inputView i ActInput]
        ModelProcess i -> [processView i ActProcess]
        ModelCmp i -> [cmpView i ActCmp]
        ModelResult m -> [ul_ [] (map (li_ [] . (:[]) . text . ms) m)]

data UIAction
    = Nope
    | ActInput InputAction
    | ActProcess ProcessAction
    | ActCmp (Action MisoString)

update' :: UIAction -> TopModel -> Effect UIAction TopModel
update' a m = case a of
    Nope -> noEff m
    ActInput ia ->
        -- FIXME: Partial match. need to associate ActInput with ModelInput, etc.
        let ModelInput im = m
            wrap BeginSort d = noEff (ModelProcess d)
            -- FIXME: Need to distinguish internally-handled actions with
            -- ejectable actions.
            wrap _ _ = error "Unhandled wrapped ActInput action"
        in inputUpdate ia im ModelInput wrap
    ActProcess pa ->
        let ModelProcess pm = m
            wrap Back d = noEff (ModelInput d)
            wrap (Sort is) _ =
                case firstCmp is of
                    Left xs -> noEff (ModelResult xs)
                    Right ms -> noEff (ModelCmp (CmpModel [] ms Nothing))
        in processUpdate pa pm ModelProcess wrap
    ActCmp act ->
        let ModelCmp cm = m
            wrap (Done xs) = noEff (ModelResult xs)
        in processCmp act cm ModelCmp wrap

main :: IO ()
main = startApp App {..} where
    initialAction = Nope
    model = ModelInput ""
    update = update'
    view = topView
    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off
