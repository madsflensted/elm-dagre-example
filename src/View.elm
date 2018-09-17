module View exposing (view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import GraphParser
import Html
import Html.Attributes as HA
import Parser
import Svg exposing (..)
import Svg.Attributes as SA
import Types exposing (..)


defaultGraphOptions : GraphOptions
defaultGraphOptions =
    GraphOptions True False False


defaultGraphValues : GraphValues
defaultGraphValues =
    GraphValues 10 100 "LR" 20 20 600 600


view : Model -> Browser.Document Msg
view model =
    { title = "Dagre Viewer"
    , body = [ body model ]
    }


body : Model -> Html.Html Msg
body { editor, graph } =
    Element.layout [] <|
        column
            [ width fill
            , spacing 10
            ]
            [ header
            , editorResult editor graph
            ]


header =
    paragraph
        [ padding 25
        , Background.color (rgb 120 120 120)
        ]
        [ Element.text "Specify a graph in the left pane, and see the result in the right. The layout is calculated with the external JS library"
        , newTabLink [ Font.bold ]
            { url = "https://github.com/dagrejs/dagre/wiki"
            , label = Element.text "dagre.js"
            }
        ]


editorResult editor graph =
    let
        le =
            List.length errors |> Debug.log "Number of errors"

        errors =
            graphErrors editor

        hasErrors =
            List.any (\s -> s /= "") errors
    in
    row
        [ width fill
        , spacing 10
        ]
        [ dataPane editor hasErrors
        , if hasErrors then
            errorPane errors

          else
            graphPane graph
        ]


dataPane : String -> Bool -> Element Msg
dataPane editor hasErrors =
    mLine editor "Graph specification:" hasErrors Edit


mLine : String -> String -> Bool -> (String -> Msg) -> Element Msg
mLine content lbl hasErrors msg =
    el
        [ width <| fillPortion 1
        , Font.family [ Font.monospace ]
        , padding 10
        ]
    <|
        Input.multiline
            [ HA.rows 30 |> Element.htmlAttribute
            ]
            { onChange = msg
            , text = content
            , placeholder = Nothing
            , label = Input.labelAbove [] <| Element.text lbl
            , spellcheck = False
            }


errorPane : List String -> Element Msg
errorPane errors =
    let
        content =
            String.join "\n" errors
    in
    mLine content "Errors in specification:" False (\_ -> NoOp)


graphPane : Maybe GraphData -> Element msg
graphPane graph =
    let
        defaultGraph =
            GraphData defaultGraphOptions defaultGraphValues [] []

        graphData =
            Maybe.withDefault defaultGraph graph

        sw =
            String.fromInt graphData.values.width

        sh =
            String.fromInt graphData.values.height
    in
    viewNodes graphData.nodes
        |> (++) (viewEdges graphData.edges)
        |> svg [ SA.width sw, SA.height sh, SA.viewBox ("0 0 " ++ sw ++ " " ++ sh) ]
        |> Element.html
        |> el [ centerX, alignTop, width <| fillPortion 1 ]


viewNodes : List Node -> List (Svg msg)
viewNodes nodes =
    List.map viewNode nodes


viewNode : Node -> Svg msg
viewNode node =
    let
        mx =
            String.fromFloat <| node.value.x

        my =
            String.fromFloat <| node.value.y

        sx =
            String.fromFloat <| node.value.x - (toFloat node.value.width / 2.0)

        sy =
            String.fromFloat <| node.value.y - (toFloat node.value.height / 2.0)

        sh =
            String.fromInt node.value.height

        sw =
            String.fromInt node.value.width
    in
    Svg.node "g"
        []
        [ rect
            [ SA.x sx
            , SA.y sy
            , SA.width sw
            , SA.height sh
            , SA.rx "10"
            , SA.ry "10"
            , SA.stroke "blue"
            , SA.fill "white"
            ]
            []
        , text_
            [ SA.x mx
            , SA.y my
            , SA.alignmentBaseline "central"
            , SA.textAnchor "middle"
            ]
            [ Svg.text node.value.label ]
        ]


viewEdges : List Edge -> List (Svg msg)
viewEdges edges =
    List.map viewEdge edges


viewEdge : Edge -> Svg msg
viewEdge edge =
    let
        mx =
            String.fromFloat edge.values.x

        my =
            String.fromFloat edge.values.y

        sx { x } =
            String.fromFloat x

        sy { y } =
            String.fromFloat y

        p2s p =
            sx p ++ "," ++ sy p

        pnts =
            String.join " " <| List.map p2s edge.values.points
    in
    node "g"
        []
        [ polyline [ SA.fill "none", SA.stroke "black", SA.points pnts ] []
        , text_
            [ SA.x mx
            , SA.y my
            , SA.alignmentBaseline "central"
            , SA.textAnchor "middle"
            ]
            [ Svg.text edge.values.label ]
        ]


graphErrors : String -> List String
graphErrors str =
    let
        parsed =
            GraphParser.parse str

        getError x =
            case x of
                Ok _ ->
                    ""

                Err err ->
                    String.join " or " <| List.map (\{ col, problem } -> Debug.toString problem ++ " at column " ++ String.fromInt col) err
    in
    List.map getError parsed
