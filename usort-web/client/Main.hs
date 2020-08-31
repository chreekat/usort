{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso
import Miso.String

type Model = Int

data Action = Start | Bump

main :: IO ()
main = startApp App {..} where
    initialAction = Start
    model = 0
    update = update'
    view = view'
    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off

update' a m = case a of
    Start -> noEff m
    Bump -> noEff (m + 1)

view' :: Model -> View Action
view' m =
    div_ []
        [ button_ [ onClick Bump ] [ text "Bump" ]
        , text (ms m)
        ]
