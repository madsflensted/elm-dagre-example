module View exposing (view)

import Bootstrap.Form as Form
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import GraphParser
import Html
import Html.Attributes as HA
import Svg exposing (..)
import Svg.Attributes as SA
import Types exposing (..)


defaultGraphOptions : GraphOptions
defaultGraphOptions =
    GraphOptions True False False


defaultGraphValues : GraphValues
defaultGraphValues =
    GraphValues 10 100 "LR" 20 20 600 600


view : Model -> Html.Html Msg
view { editor, graph } =
    let
        errors =
            graphErrors editor

        hasErrors =
            List.any (\s -> s /= "") errors
    in
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col []
                [ Html.h1 []
                    [ text "A slice of Elm "
                    , Html.small [ HA.attribute "class" "text-muted" ] [ Html.em [] [ text "Beta" ] ]
                    ]
                , Html.p []
                    [ text "Specify a graph in the left pane, and see the result in the right. The layout is calculated with the external JS library"
                    , Html.a [ HA.href "https://github.com/dagrejs/dagre/wiki" ] [ text " dagre.js." ]
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col [] [ dataPane editor hasErrors ]
            , if hasErrors then
                Grid.col [] [ errorPane errors ]
              else
                Grid.col [] [ graphPane graph ]
            ]
        ]


dataPane : String -> Bool -> Html.Html Msg
dataPane editor hasErrors =
    Form.form []
        [ Form.group []
            [ Form.label [] [ text "Graph specification:" ]
            , Textarea.textarea
                ([ Textarea.id "data"
                 , Textarea.rows 20
                 , Textarea.onInput Edit
                 , Textarea.value editor
                 ]
                    ++ (if hasErrors then
                            [ Textarea.danger ]
                        else
                            []
                       )
                )
            ]
        ]


errorPane : List String -> Html.Html Msg
errorPane errors =
    Form.form []
        [ Form.group []
            [ Form.label [] [ text "Parse errors:" ]
            , Textarea.textarea
                [ Textarea.id "data"
                , Textarea.rows 20
                , Textarea.disabled
                , Textarea.attrs
                    [ HA.style
                        [ ( "overflow-x", "scroll" )
                        ]
                    ]
                , Textarea.value (String.join "\n" errors)
                ]
            ]
        ]


graphPane : Maybe GraphData -> Html.Html msg
graphPane graph =
    let
        defaultGraph =
            GraphData defaultGraphOptions defaultGraphValues [] []

        graphData =
            Maybe.withDefault defaultGraph graph

        sw =
            toString graphData.values.width

        sh =
            toString graphData.values.height
    in
    Grid.container []
        [ Grid.row [ Row.attrs [ HA.attribute "class" "py-3" ] ]
            [ Grid.col []
                [ svg
                    [ SA.width sw, SA.height sh, SA.viewBox ("0 0 " ++ sw ++ " " ++ sh) ]
                  <|
                    viewNodes graphData.nodes
                        ++ viewEdges graphData.edges
                ]
            ]
        ]


viewNodes : List Node -> List (Svg msg)
viewNodes nodes =
    List.map viewNode nodes


viewNode : Node -> Svg msg
viewNode node =
    let
        mx =
            toString <| node.value.x

        my =
            toString <| node.value.y

        sx =
            toString <| node.value.x - (toFloat node.value.width / 2.0)

        sy =
            toString <| node.value.y - (toFloat node.value.height / 2.0)

        sh =
            toString node.value.height

        sw =
            toString node.value.width
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
            [ text node.value.label ]
        ]


viewEdges : List Edge -> List (Svg msg)
viewEdges edges =
    List.map viewEdge edges


viewEdge : Edge -> Svg msg
viewEdge edge =
    let
        mx =
            toString edge.values.x

        my =
            toString edge.values.y

        sx { x } =
            toString x

        sy { y } =
            toString y

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
            [ text edge.values.label ]
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
                    err.source
                        ++ " : "
                        ++ toString err.problem
    in
    List.map getError parsed
