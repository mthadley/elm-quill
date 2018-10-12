module Quill.Attribute exposing (Attribute(..), decode, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| The built in Quill attributes, like "bold" and "italics".
-}
type Attribute
    = Bold
    | Italic
    | Link String
    | Background String


decode : String -> Decoder Attribute
decode pair =
    case pair of
        "bold" ->
            Decode.succeed Bold

        "italic" ->
            Decode.succeed Italic

        "link" ->
            Decode.map Link Decode.string

        "background" ->
            Decode.map Background Decode.string

        name ->
            Decode.fail <| "Unsupported attribute \"" ++ name ++ "\""


encode : Attribute -> ( String, Encode.Value )
encode attr =
    case attr of
        Bold ->
            ( "bold", Encode.bool True )

        Italic ->
            ( "italic", Encode.bool True )

        Link url ->
            ( "link", Encode.string url )

        Background color ->
            ( "background", Encode.string color )
