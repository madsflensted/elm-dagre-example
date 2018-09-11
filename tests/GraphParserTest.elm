module GraphParserTest exposing (..)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect exposing (Expectation)
import GraphParser exposing (parse)
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "The Graph Parser"
        [ test "parse simple node" <|
            \_ ->
                Expect.equal (parse "A: Kevin")
                    [ Ok (N "A" "Kevin") ]
        , test "parse label with space" <|
            \_ ->
                Expect.equal (parse "A : Kevin Spacey")
                    [ Ok (N "A" "Kevin Spacey") ]
        , test "parse edge" <|
            \_ ->
                Expect.equal (parse "A -> B : worked with")
                    [ Ok (E "A" "B" "worked with") ]
        , test "parse edge and node" <|
            \_ ->
                Expect.equal (parse "A:foo\nB : bar\nA -> B : worked with")
                    [ Ok (N "A" "foo")
                    , Ok (N "B" "bar")
                    , Ok (E "A" "B" "worked with")
                    ]
        ]
