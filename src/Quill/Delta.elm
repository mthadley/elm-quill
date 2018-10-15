module Quill.Delta exposing
    ( Blot(..)
    , Delta
    , Op(..)
    , cons
    , decode
    , encode
    , format
    , fromList
    , fromString
    , init
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Quill.Attribute as Attribute exposing (Attribute)
import Quill.Attribute.Image as Image
import Quill.Range exposing (Range)


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
    | Image String (List Image.Attribute)
    | Unsupported Decode.Value



-- CREATION


init : Delta attr
init =
    Delta [ Insert <| Text "\n" [] ]


fromList : List (Op attr) -> Delta attr
fromList =
    Delta


fromString : String -> Delta attr
fromString text =
    Delta [ Insert <| Text text [] ]



-- HELPERS


cons : Op attr -> Delta attr -> Delta attr
cons op (Delta ops) =
    Delta (op :: ops)


append : Delta attr -> Delta attr -> Delta attr
append (Delta a) (Delta b) =
    Delta (a ++ b)


format : Attribute attr -> Range -> Delta attr -> Delta attr
format attr range ((Delta someOps) as delta) =
    case ( range.length, someOps ) of
        ( 0, _ ) ->
            delta

        ( _, [] ) ->
            delta

        ( length, op :: rest ) ->
            if range.index < opLength op then
                append
                    (fromList <| applyFormat attr range op)
                    (format
                        attr
                        { length = max (length - (opLength op - range.index)) 0
                        , index = max (range.index - opLength op) 0
                        }
                        (fromList rest)
                    )

            else
                append
                    (fromList [ op ])
                    (format
                        attr
                        { range | index = range.index - opLength op }
                        (fromList rest)
                    )


applyFormat : Attribute attr -> Range -> Op attr -> List (Op attr)
applyFormat attr { index, length } op =
    case op of
        Insert blot ->
            case blot of
                Text text attrs ->
                    [ Insert <| Text (String.left index text) attrs
                    , Insert <| Text (String.slice index (index + length) text) <| attrs ++ [ attr ]
                    , Insert <| Text (String.dropLeft (index + length) text) attrs
                    ]

                Image _ _ ->
                    [ op ]

                Unsupported _ ->
                    [ op ]

        Delete _ ->
            [ op ]

        Retain retainLength attrs ->
            [ op ]


opLength : Op attr -> Int
opLength op =
    case op of
        Insert blot ->
            case blot of
                Text string _ ->
                    String.length string

                Image _ _ ->
                    1

                Unsupported _ ->
                    1

        Delete length ->
            length

        Retain length _ ->
            length


toString : Delta attr -> String
toString (Delta ops) =
    let
        opToString op =
            case op of
                Insert blot ->
                    case blot of
                        Text text _ ->
                            text

                        Image _ _ ->
                            ""

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

        Image url attrs ->
            Encode.object
                [ ( "insert", Encode.object [ ( "image", Encode.string url ) ] )
                , ( "attributes", Encode.object <| List.map Image.encode attrs )
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
        decodeAttrs =
            decodeAttrsWith (Attribute.decode attrDecoder)
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
            , Decode.map2 Image
                (Decode.at [ "insert", "image" ] Decode.string)
                (decodeAttrsWith Image.decode)
            , Decode.map Unsupported Decode.value
            ]


decodeAttrsWith : (String -> Decoder a) -> Decoder (List a)
decodeAttrsWith decoder =
    let
        decodeAttr ( key, value ) =
            value
                |> Decode.decodeValue (decoder key)
                |> Result.toMaybe
    in
    Decode.keyValuePairs Decode.value
        |> Decode.field "attributes"
        |> Decode.maybe
        |> Decode.map (Maybe.withDefault [] >> List.filterMap decodeAttr)
