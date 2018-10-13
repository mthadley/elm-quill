module Quill.Attribute exposing (Attribute(..), decode, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Attribute attr
    = Bold
    | Italic
    | Underline
    | Link String
    | Background String
    | Custom attr


decode : (String -> Decoder attr) -> String -> Decoder (Attribute attr)
decode decodeAttr pair =
    case pair of
        "bold" ->
            Decode.succeed Bold

        "italic" ->
            Decode.succeed Italic

        "underline" ->
            Decode.succeed Underline

        "link" ->
            Decode.map Link Decode.string

        "background" ->
            Decode.map Background Decode.string

        name ->
            Decode.map Custom (decodeAttr name)


encode : (attr -> ( String, Encode.Value )) -> Attribute attr -> ( String, Encode.Value )
encode encodeCustom attr =
    case attr of
        Bold ->
            ( "bold", Encode.bool True )

        Italic ->
            ( "italic", Encode.bool True )

        Underline ->
            ( "underline", Encode.bool True )

        Link url ->
            ( "link", Encode.string url )

        Background color ->
            ( "background", Encode.string color )

        Custom customAttr ->
            encodeCustom customAttr
