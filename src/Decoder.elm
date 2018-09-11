module Decoder exposing (decodeGraph)

import Json.Decode exposing (Decoder, Value, bool, decodeValue, float, int, list, string)
import Json.Decode.Pipeline exposing (custom, decode, optional, required)
import Types exposing (..)


decodeGraph : Value -> Result String GraphData
decodeGraph json =
    decodeValue graphData json


graphData : Decoder GraphData
graphData =
    decode GraphData
        |> required "options" graphOptions
        |> required "value" graphValues
        |> required "nodes" (list node)
        |> required "edges" (list edge)


graphOptions : Decoder GraphOptions
graphOptions =
    decode GraphOptions
        |> required "directed" bool
        |> required "multigraph" bool
        |> required "compound" bool


graphValues : Decoder GraphValues
graphValues =
    decode GraphValues
        |> required "nodesep" int
        |> required "ranksep" int
        |> required "rankdir" string
        |> required "marginx" int
        |> required "marginy" int
        |> required "width" int
        |> required "height" int


node : Decoder Node
node =
    decode Node
        |> required "v" string
        |> required "value" nodeValue


nodeValue : Decoder NodeValue
nodeValue =
    decode NodeValue
        |> required "label" string
        |> required "height" int
        |> required "width" int
        |> required "x" float
        |> required "y" float


edge : Decoder Edge
edge =
    decode Edge
        |> required "v" string
        |> required "w" string
        |> required "value" edgeValue


edgeValue : Decoder EdgeValue
edgeValue =
    decode EdgeValue
        |> required "label" string
        |> required "height" int
        |> required "width" int
        |> required "x" float
        |> required "y" float
        |> required "points" (list point)


point : Decoder Point
point =
    decode Point
        |> required "x" float
        |> required "y" float
