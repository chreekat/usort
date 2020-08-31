{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Miso
import Miso.String

-- * Model shtuff

data TopMode list res = Input | Process list | Result res
deriving instance (Show list, Show res) => Show (TopMode list res)
deriving instance (Eq list, Eq res) => Eq (TopMode list res)

data Model list res = Model
    { appMode :: TopMode list res
    , input :: MisoString
    }

deriving instance (Show list, Show res) => Show (Model list res)
deriving instance (Eq list, Eq res) => Eq (Model list res)

-- * View shtuff

inputView :: View Action
inputView =
    div_ []
        [ h1_ [] [text "U Sort It"]
        , textarea_
            [ wrap_ "off", cols_ "80", rows_ "50"
            , onInput UpdateInput
            ]
            [ text "Put yo shit here"]
        , button_ [ onClick BeginSort ] [text "Click"]
        ]

sortingView ::  MisoString -> View Action
sortingView list =
    div_ []
        [ div_ [] [text "You want me to sort this, yeah?"]
        , pre_ [] [text list]
        ]

topView m = case appMode m of
    Input -> inputView
    Process list -> sortingView list


data Action = Nope | UpdateInput MisoString | BeginSort

main :: IO ()
main = startApp App {..} where
    initialAction = Nope
    model = Model Input "" :: Model MisoString ()
    update = update'
    view = topView
    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off

update' a m = case a of
    Nope -> noEff m
    UpdateInput s -> noEff m { input = s }
    BeginSort -> noEff m { appMode = Process (input m) }
