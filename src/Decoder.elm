module Decoder exposing (decodeGraph)

import Json.Decode exposing (Decoder, Error, Value, bool, decodeValue, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Types exposing (..)


decodeGraph : Value -> Result Error GraphData
decodeGraph json =
    decodeValue graphData json


graphData : Decoder GraphData
graphData =
    succeed GraphData
        |> required "options" graphOptions
        |> required "value" graphValues
        |> required "nodes" (list node)
        |> required "edges" (list edge)


graphOptions : Decoder GraphOptions
graphOptions =
    succeed GraphOptions
        |> required "directed" bool
        |> required "multigraph" bool
        |> required "compound" bool


graphValues : Decoder GraphValues
graphValues =
    succeed GraphValues
        |> required "nodesep" int
        |> required "ranksep" int
        |> required "rankdir" string
        |> required "marginx" int
        |> required "marginy" int
        |> required "width" int
        |> required "height" int


node : Decoder Node
node =
    succeed Node
        |> required "v" string
        |> required "value" nodeValue


nodeValue : Decoder NodeValue
nodeValue =
    succeed NodeValue
        |> required "label" string
        |> required "height" int
        |> required "width" int
        |> required "x" float
        |> required "y" float


edge : Decoder Edge
edge =
    succeed Edge
        |> required "v" string
        |> required "w" string
        |> required "value" edgeValue


edgeValue : Decoder EdgeValue
edgeValue =
    succeed EdgeValue
        |> required "label" string
        |> required "height" int
        |> required "width" int
        |> required "x" float
        |> required "y" float
        |> required "points" (list point)


point : Decoder Point
point =
    succeed Point
        |> required "x" float
        |> required "y" float
