module Quill.Delta exposing
    ( Blot(..)
    , Delta
    , Op(..)
    , decode
    , encode
    , fromList
    , init
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Quill.Attribute as Attribute exposing (Attribute)


type Delta attr
    = Delta (List (Op attr))


{-| Not all Blots support the same kinds of attributes. If we decide to
support more than just text in the future, we should adjust the types to
reflect this.
-}
type Op attr
    = Insert (Blot attr)
    | Delete Int
    | Retain Int (List (Attribute attr))


type Blot attr
    = Text String (List (Attribute attr))
    | Unsupported Decode.Value



-- CREATION


init : Delta attr
init =
    Delta [ Insert (Text "\n" []) ]


fromList : List (Op attr) -> Delta attr
fromList =
    Delta



-- HELPERS


toString : Delta attr -> String
toString (Delta ops) =
    let
        opToString op =
            case op of
                Insert blot ->
                    case blot of
                        Text text _ ->
                            text

                        Unsupported _ ->
                            ""

                Delete _ ->
                    ""

                Retain _ _ ->
                    ""
    in
    ops
        |> List.map opToString
        |> String.join ""



-- CODECS


encode : (attr -> ( String, Encode.Value )) -> Delta attr -> Encode.Value
encode attrEncoder (Delta attrs) =
    Encode.object [ ( "ops", Encode.list (encodeOp attrEncoder) attrs ) ]


encodeOp : (attr -> ( String, Encode.Value )) -> Op attr -> Encode.Value
encodeOp attrEncoder op =
    case op of
        Insert blot ->
            encodeBlot attrEncoder blot

        Delete count ->
            Encode.object [ ( "delete", Encode.int count ) ]

        Retain count attrs ->
            Encode.object
                [ ( "retain", Encode.int count )
                , ( "attributes", encodeAttrs attrEncoder attrs )
                ]


encodeAttrs : (attr -> ( String, Encode.Value )) -> List (Attribute attr) -> Encode.Value
encodeAttrs attrEncoder =
    List.map (Attribute.encode attrEncoder) >> Encode.object


encodeBlot : (attr -> ( String, Encode.Value )) -> Blot attr -> Encode.Value
encodeBlot attrEncoder blot =
    case blot of
        Text text attrs ->
            Encode.object
                [ ( "insert", Encode.string text )
                , ( "attributes", encodeAttrs attrEncoder attrs )
                ]

        Unsupported value ->
            value


decode : (String -> Decoder attr) -> Decoder (Delta attr)
decode attrDecoder =
    decodeOp attrDecoder
        |> Decode.list
        |> Decode.field "ops"
        |> Decode.map Delta


decodeOp : (String -> Decoder attr) -> Decoder (Op attr)
decodeOp attrDecoder =
    let
        decodeAttr ( key, value ) =
            value
                |> Decode.decodeValue (Attribute.decode attrDecoder key)
                |> Result.toMaybe

        decodeAttrs =
            Decode.keyValuePairs Decode.value
                |> Decode.field "attributes"
                |> Decode.maybe
                |> Decode.map (Maybe.withDefault [] >> List.filterMap decodeAttr)
    in
    Decode.oneOf
        [ decodeInsert decodeAttrs
        , Decode.map Delete (Decode.field "delete" Decode.int)
        , Decode.map2 Retain (Decode.field "retain" Decode.int) decodeAttrs
        ]


decodeInsert : Decoder (List (Attribute attr)) -> Decoder (Op attr)
decodeInsert attrsDecoder =
    Decode.map Insert <|
        Decode.oneOf
            [ Decode.map2 Text
                (Decode.field "insert" Decode.string)
                attrsDecoder
            , Decode.map Unsupported Decode.value
            ]
