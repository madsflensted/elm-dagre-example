module GraphParser exposing (parse)

import Char
import Parser exposing (..)
import Types exposing (..)


parse : String -> List (Result (List DeadEnd) Entry)
parse str =
    str
        |> String.lines
        |> List.map (run parseLine)


parseLine : Parser Entry
parseLine =
    oneOf
        [ parseNodeOrEdge
        , parseComment
        , parseBlank
        ]


parseComment : Parser Entry
parseComment =
    succeed C
        |. symbol "#"
        |. spaces
        |= (getChompedString <|
                (succeed () |. chompUntilEndOr "\n")
           )
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


parseNodeOrEdge : Parser Entry
parseNodeOrEdge =
    succeed addFirstName
        |. spaces
        |= parseName
        |. spaces
        |= (oneOf
                [ succeed True |. symbol ":"
                , succeed False |. symbol "->"
                ]
                |> andThen
                    (\v ->
                        if v then
                            succeed (\l -> N "" l)
                                |. spaces
                                |= parseLabel
                                |. spaces

                        else
                            succeed (\n2 l -> E "" n2 l)
                                |. spaces
                                |= parseName
                                |. spaces
                                |. symbol ":"
                                |. spaces
                                |= parseLabel
                                |. spaces
                    )
           )


addFirstName : String -> Entry -> Entry
addFirstName n e =
    case e of
        N _ l ->
            N n l

        E _ n2 l ->
            E n n2 l

        x ->
            x


parseLabel : Parser String
parseLabel =
    getChompedString <|
        succeed ()
            |. chompWhile isLabelChar


parseName : Parser String
parseName =
    getChompedString <|
        succeed ()
            |. chompWhile isNameChar


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
    succeed ()
        |. chompWhile (\c -> c == ' ')
