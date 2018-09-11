module Encoder exposing (graphToJsonStr)

import GraphParser
import Json.Encode exposing (Value, bool, encode, float, int, list, object, string)
import Parser exposing (Error)
import Types exposing (..)


graphToJsonStr : String -> String
graphToJsonStr str =
    encode 0 <|
        object
            [ ( "nodes", nodesJson str )
            , ( "edges", edgesJson str )
            , ( "options", jsonOptions )
            , ( "value", jsonGraphValues )
            ]


parsed : String -> List (Result Error Entry)
parsed str =
    GraphParser.parse str


jsonNode : String -> String -> Int -> Int -> Value
jsonNode a l w h =
    object
        [ ( "v", string a )
        , ( "value"
          , object
                [ ( "label", string l )
                , ( "width", int w )
                , ( "height", int h )
                ]
          )
        ]


jsonEdge : String -> String -> String -> Int -> Int -> Value
jsonEdge a b l w h =
    object
        [ ( "v", string a )
        , ( "w", string b )
        , ( "value"
          , object
                [ ( "label", string l )
                , ( "width", int w )
                , ( "height", int h )
                ]
          )
        ]


nodeToJson : Result Error Entry -> Maybe Value
nodeToJson n =
    case n of
        Ok (N a l) ->
            Just <| jsonNode a l 140 50

        _ ->
            Nothing


edgeToJson : Result Error Entry -> Maybe Value
edgeToJson e =
    case e of
        Ok (E a b l) ->
            Just <| jsonEdge a b l 90 10

        _ ->
            Nothing


jsonOptions : Value
jsonOptions =
    object
        [ ( "directed", bool True )
        , ( "multigraph", bool False )
        , ( "compound", bool False )
        ]


jsonGraphValues : Value
jsonGraphValues =
    object
        [ ( "nodesep", int 10 )
        , ( "ranksep", int 100 )
        , ( "rankdir", string "LR" )
        , ( "marginx", int 20 )
        , ( "marginy", int 20 )
        ]


nodesJson : String -> Value
nodesJson str =
    list (List.filterMap nodeToJson <| parsed str)


edgesJson : String -> Value
edgesJson str =
    list (List.filterMap edgeToJson <| parsed str)
