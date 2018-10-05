module Quill.Delta exposing (Delta, decode, encode, init)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Delta =
    List Op


type Op
    = Insert String
    | Delete Int
    | Retain Int


init : Delta
init =
    [ Insert "" ]


encode : Delta -> Encode.Value
encode delta =
    Encode.object [ ( "ops", Encode.list encodeOp delta ) ]


encodeOp : Op -> Encode.Value
encodeOp op =
    case op of
        Insert string ->
            Encode.object [ ( "insert", Encode.string string ) ]

        Delete count ->
            Encode.object [ ( "delete", Encode.int count ) ]

        Retain count ->
            Encode.object [ ( "retain", Encode.int count ) ]


decode : Decoder Delta
decode =
    Decode.field "ops" (Decode.list decodeOp)


decodeOp : Decoder Op
decodeOp =
    Decode.oneOf
        [ Decode.map Insert (Decode.field "insert" Decode.string)
        , Decode.map Delete (Decode.field "delete" Decode.int)
        , Decode.map Retain (Decode.field "retain" Decode.int)
        ]
