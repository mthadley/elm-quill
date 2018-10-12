module Quill.Delta exposing
    ( Delta
    , Op(..)
    , decode
    , encode
    , fromList
    , init
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Quill.Attribute exposing (AttrDecoder, AttrEncoder)


type Delta attr
    = Delta (List (Op attr))


type Op attr
    = Insert String (List attr)
    | Delete Int
    | Retain Int (List attr)



-- CREATION


init : Delta attr
init =
    Delta [ Insert "" [] ]


fromList : List (Op attr) -> Delta attr
fromList =
    Delta



-- HELPERS


toString : Delta attr -> String
toString (Delta ops) =
    let
        opToString op =
            case op of
                Insert value _ ->
                    value

                Delete _ ->
                    ""

                Retain _ _ ->
                    ""
    in
    ops
        |> List.map opToString
        |> String.join ""



-- CODECS


encode : AttrEncoder attr -> Delta attr -> Encode.Value
encode attrEncoder (Delta attrs) =
    Encode.object [ ( "ops", Encode.list (encodeOp attrEncoder) attrs ) ]


encodeOp : AttrEncoder attr -> Op attr -> Encode.Value
encodeOp attrEncoder op =
    case op of
        Insert string attrs ->
            Encode.object
                [ ( "insert", Encode.string string )
                , ( "attributes", encodeAttrs attrEncoder attrs )
                ]

        Delete count ->
            Encode.object [ ( "delete", Encode.int count ) ]

        Retain count attrs ->
            Encode.object
                [ ( "retain", Encode.int count )
                , ( "attributes", encodeAttrs attrEncoder attrs )
                ]


encodeAttrs : AttrEncoder attr -> List attr -> Encode.Value
encodeAttrs attrEncoder =
    List.map attrEncoder >> Encode.object


decode : AttrDecoder attr -> Decoder (Delta attr)
decode attrDecoder =
    decodeOp attrDecoder
        |> Decode.list
        |> Decode.field "ops"
        |> Decode.map Delta


decodeOp : AttrDecoder attr -> Decoder (Op attr)
decodeOp attrDecoder =
    let
        decodeAttrs =
            Decode.keyValuePairs Decode.value
                |> Decode.field "attributes"
                |> Decode.maybe
                |> Decode.map (Maybe.withDefault [] >> List.filterMap attrDecoder)
    in
    Decode.oneOf
        [ Decode.map2 Insert (Decode.field "insert" Decode.string) decodeAttrs
        , Decode.map Delete (Decode.field "delete" Decode.int)
        , Decode.map2 Retain (Decode.field "retain" Decode.int) decodeAttrs
        ]
