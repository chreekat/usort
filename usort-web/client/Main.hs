{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Miso
import Miso.String

-- * Input app

type InputModel = JSString

-- inputView :: View Action
inputView m wrap =
    div_ []
        [ textarea_
            [ wrap_ "off", cols_ "80", rows_ "50"
            , onInput (wrap . UpdateInput)
            ]
            [ text (ms m) ]
        , button_ [ onClick (wrap BeginSort) ] [text "Click"]
        ]

data InputAction = UpdateInput JSString | BeginSort

inputUpdate a m wrapModel ejectAct = case a of
    UpdateInput j -> noEff (wrapModel j)
    BeginSort -> ejectAct a m

-- * Process app

type ProcessModel = JSString

-- processView :: MisoString -> View Action
processView stuff wrap =
    div_ []
        [ div_ [] [text "You want me to sort this, yeah?"]
        , ul_ [] (fmap (li_ [] . (:[]) . text) (Miso.String.lines stuff))
        , button_ [ onClick (wrap Back) ] [text "No"]
        ]

data ProcessAction = Back

-- processUpdate :: ProcessAction -> ProcessModel -> Effect a m
processUpdate a m _wrapModel ejectAct = case a of
    Back -> ejectAct a m

-- * Result app

-- No findings :<

-- * Top level composition

data TopModel = ModeInput InputModel | ModeProcess ProcessModel | ModeResult ResultModel
    deriving (Eq, Show)

type ResultModel = [JSString]

topView :: TopModel -> View Action
topView m =
    div_ []
        [ h1_ [] [text "U Sort It"]
        , div_ [] rest
        ]
  where
    rest =
        case m of
        ModeInput i -> [inputView i ActInput]
        ModeProcess i -> [processView i ActProcess]

data Action = Nope | ActInput InputAction | ActProcess ProcessAction

update' :: Action -> TopModel -> Effect Action TopModel
update' a m = case a of
    Nope -> noEff m
    ActInput ia ->
        let ModeInput im = m
            wrap BeginSort d = noEff (ModeProcess d)
            wrap _ _ = error "Unhandled wrapped ActInput action"
        in inputUpdate ia im ModeInput wrap
    ActProcess pa ->
        let ModeProcess pm = m
            wrap Back d = noEff (ModeInput d)
        in processUpdate pa pm ModeProcess wrap

main :: IO ()
main = startApp App {..} where
    initialAction = Nope
    model = ModeInput ""
    update = update'
    view = topView
    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off
