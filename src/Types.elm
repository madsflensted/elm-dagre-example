module Types exposing (Edge, EdgeValue, Entry(..), GraphData, GraphOptions, GraphValues, Model, Msg(..), Node, NodeValue, Point)

import Json.Encode exposing (Value)


type Msg
    = Edit String
    | Graph Value
    | Layout
    | NoOp


type alias Model =
    { graph : Maybe GraphData
    , editor : String
    , layoutRequested : Int
    }



-- Graph


type alias GraphData =
    { options : GraphOptions
    , values : GraphValues
    , nodes : List Node
    , edges : List Edge
    }


type alias GraphOptions =
    { directed : Bool
    , multigraph : Bool
    , compound : Bool
    }


type alias GraphValues =
    { nodesep : Int
    , ranksep : Int
    , rankdir : String
    , marginx : Int
    , marginy : Int
    , width : Int
    , height : Int
    }


type alias Node =
    { id : String
    , value : NodeValue
    }


type alias NodeValue =
    { label : String
    , height : Int
    , width : Int
    , x : Float
    , y : Float
    }


type alias Edge =
    { from : String
    , to : String
    , values : EdgeValue
    }


type alias EdgeValue =
    { label : String
    , height : Int
    , width : Int
    , x : Float
    , y : Float
    , points : List Point
    }


type alias Point =
    { x : Float
    , y : Float
    }



-- Parsed lines


type Entry
    = N String String
    | E String String String
    | C String
    | Blank
