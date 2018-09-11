module GraphParser exposing (parse)

import Char
import Parser exposing (..)
import Types exposing (..)


parse : String -> List (Result Error Entry)
parse str =
    str
        |> String.lines
        |> List.map (run parseLine)


parseLine : Parser Entry
parseLine =
    oneOf
        [ parseEdge
        , parseNode
        , parseComment
        , parseBlank
        ]


parseComment : Parser Entry
parseComment =
    succeed C
        |. symbol "#"
        |. spaces
        |= keep zeroOrMore (\_ -> True)
        |. end


parseBlank : Parser Entry
parseBlank =
    succeed Blank
        |. end



{-
   Node:
     foo : Fooish label
     bar : Barish label

   Edge:
     foo -> bar : Relation label
-}


parseNode : Parser Entry
parseNode =
    delayedCommitMap N parseName <|
        succeed identity
            |. symbol ":"
            |. spaces
            |= keep oneOrMore isLabelChar


parseEdge : Parser Entry
parseEdge =
    delayedCommitMap (\a ( b, c ) -> E a b c) parseName <|
        succeed (,)
            |. symbol "->"
            |. spaces
            |= keep oneOrMore isNameChar
            |. spaces
            |. symbol ":"
            |. spaces
            |= keep oneOrMore isLabelChar


parseName : Parser String
parseName =
    keep oneOrMore isNameChar
        |. spaces


isNameChar : Char -> Bool
isNameChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || char
        == '_'


isLabelChar : Char -> Bool
isLabelChar char =
    isNameChar char
        || char
        == ' '


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')
