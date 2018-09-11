port module Main exposing (main)

import Decoder exposing (decodeGraph)
import Encoder exposing (graphToJsonStr)
import Html
import Json.Encode exposing (Value)
import Process
import Task
import TestGraph
import Time
import Types exposing (..)
import View exposing (view)


-- Init


init : ( Model, Cmd msg )
init =
    let
        model =
            Model Nothing TestGraph.data 0
    in
    ( model, layout (graphToJsonStr model.editor) )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Edit str ->
            ( { model | editor = str, layoutRequested = model.layoutRequested + 1 }
            , Task.perform
                (\_ -> Layout)
                (Process.sleep (1 * Time.second))
            )

        Graph data ->
            let
                graph =
                    case decodeGraph data of
                        Ok g ->
                            Just g

                        Err e ->
                            Debug.log ("Decode failed" ++ e) Nothing
            in
            ( { model | graph = graph }, Cmd.none )

        Layout ->
            let
                outstanding =
                    if model.layoutRequested > 0 then
                        model.layoutRequested - 1
                    else
                        0
            in
            ( { model | layoutRequested = model.layoutRequested - 1 }
            , if outstanding > 0 then
                Cmd.none
              else
                layout (graphToJsonStr model.editor)
            )



-- Ports


port graphs : (Value -> msg) -> Sub msg


port layout : String -> Cmd msg



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    graphs Graph



-- Main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
