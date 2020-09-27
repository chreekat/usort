{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List.NonEmpty (NonEmpty (..))

import SplitItems
import qualified USort

import Data.Proxy
import Miso
import Miso.String (toMisoString, fromMisoString, ms, MisoString)
import Servant.API
import Servant.Links
import qualified Data.Text as T

-- jsaddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif


data Model' a = Model
    { initialInput :: a
    , initialLines :: [a]
    , cmpHistory :: [USort.MergeState a]
    , cmpMergeState :: Maybe (USort.MergeState a)
    , cmpEditBuf :: Maybe a
    , cmpResult :: [a]
    , uri :: URI
    } deriving (Eq, Show)

type Model = Model' MisoString

initModel :: URI -> Model
initModel u = Model "" [] [] Nothing Nothing [] u

data Action' a
    = Nope
    | UpdateInput MisoString
    | ConfirmInput
    | Edit USort.Choice
    | StartCompare
    | CompareAct (USort.Action a)
    | ChangeURI URI
    | HandleURI URI

type Action = Action' MisoString

type API = Input' :<|> Confirm' :<|> Compare' :<|> Done'
type Input' = View Action
type Confirm' = "confirm" :> View Action
type Compare' = "compare" :> View Action
type Done' = "results" :> View Action

update' :: Action -> Model -> Effect Action Model
update' a m = case a of
    Nope -> noEff m
    UpdateInput s -> noEff m { initialInput = s }
    ConfirmInput -> pure goConfirm' #> m
        { initialLines =
            let
                split' = items . splitItems . T.lines
                thoseLines
                    = map toMisoString
                    . split'
                    . fromMisoString
                    . initialInput
            in thoseLines m
        }
    StartCompare ->
        case USort.firstCmp (initialLines m) of
            Left xs -> pure goDone' #> m { cmpResult = xs }
            Right ms -> pure goCompare' #> m { cmpMergeState = Just ms }

    -- In usort-console, we build our own UI loop. But on the web, we just hook
    -- into the browser's loop. A user choice shows up as an 'Action'; we
    -- 'processAct' on it, and use 'ActResult' to update the model. That's, uh,
    -- all there is to it. Our model is nearly just the ActResult itself, except
    -- that we need to eject up a level in the `Left` case. Oh, we also need
    -- some scratch space for "modals" (edits, deletes).
    CompareAct act ->
        maybe
            (m <# pure goInput')
            (\(USort.ActResult newHist res) ->
                case res of
                    Left res -> pure goDone' #> m
                        { cmpHistory = newHist
                        , cmpResult = res
                        }
                    Right newMs -> noEff m
                        { cmpHistory = newHist
                        , cmpMergeState = Just newMs
                        })
            ((USort.processAct (cmpHistory m) `flip` act) <$> cmpMergeState m)
    ChangeURI u -> m { uri = u } <# do pushURI u >> pure Nope
    HandleURI u -> noEff m { uri = u }

topView :: Model -> View Action
topView m =
    div_ []
        [ h1_ [] [text "U Sort It"]
        , div_ [] [routeView]
        ]
  where
    routeView = either (const the404) id
        $ route (Proxy @ API) handlers (uri m)
    handlers = inputView :<|> confirmView :<|> cmpView :<|> doneView
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
            , ul_ []
                (fmap
                    (li_ [] . (:[]) . pre_ [] . (:[]) . text)
                    (initialLines m))
            , button_ [ onClick goInput' ] ["No"]
            , button_ [ onClick StartCompare ] ["Yes"]
            ]
    cmpView =
        let Just (USort.MergeState _ (l :| _)  (r :| _) _ d _) = cmpMergeState m
        in ul_ [] (
            [ div_ [] ["Which is more important?"]
            , li_ [] [text (ms l)]
            , li_ [] [text (ms r)]
            ]
            <> map (\(a, t) -> button_ [ onClick a ] [t])
                [ (CompareAct (USort.Choose USort.L), "Left")
                , (CompareAct (USort.Choose USort.R), "Right")
                , (CompareAct USort.Undo, "Undo")
                , (Edit USort.L, "Edit Left")
                , (Edit USort.R, "Edit Right")
                , (goInput', "Cancel sort")
                ]
        )
    doneView = ul_ [] (map (li_ [] . (:[]) . text . ms) (cmpResult m))
    the404 = div_ []
      [ text "404 = no :("
      , button_ [ onClick goInput' ] [ "go home" ]
      ]


-- | Type-safe links used in `onClick` event handlers to route the application

goInput', goConfirm', goCompare', goDone' :: Action
goInput' :<|> goConfirm' :<|> goCompare' :<|> goDone'
    = allLinks' (ChangeURI . linkURI) (Proxy @ API)

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
  Warp.runSettings
    (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings))
    =<< JSaddle.jsaddleOr
            defaultConnectionOptions
            (f >> syncPoint)
            JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

main :: IO ()
main = runApp $ do
    u <- getCurrentURI
    startApp App { model = initModel u, ..}
  where
    initialAction = Nope
    update = update'
    view = topView
    events = defaultEvents
    subs = [ uriSub HandleURI ]
    mountPoint = Nothing
    logLevel = Off
