module Quill.Range exposing (Range, debug, decode, encode, init)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Range =
    { index : Int
    , length : Int
    }


init : Range
init =
    Range 0 0


debug : Range -> String
debug { index, length } =
    "{ index = "
        ++ String.fromInt index
        ++ ", length = "
        ++ String.fromInt length
        ++ " }"


encode : Range -> Encode.Value
encode { index, length } =
    Encode.object
        [ ( "index", Encode.int index )
        , ( "length", Encode.int length )
        ]


decode : Decoder Range
decode =
    Decode.map2 Range
        (Decode.field "index" Decode.int)
        (Decode.field "length" Decode.int)
