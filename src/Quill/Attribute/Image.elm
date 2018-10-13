module Quill.Attribute.Image exposing (Attribute(..), decode, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| For whatever reason, these attributes only work when the quill editor's
format options is set to null/any. Even if we pass a whitelist that includes "image", these attributes are still not applied to the DOM.
-}
type Attribute
    = Height Int
    | Width Int
    | Alt String


decode : String -> Decoder Attribute
decode name =
    case name of
        "height" ->
            Decode.map Height decodeStringToInt

        "width" ->
            Decode.map Width decodeStringToInt

        "alt" ->
            Decode.map Alt Decode.string

        other ->
            Decode.fail <| "Unsupported image attribute \"" ++ other ++ "\""


encode : Attribute -> ( String, Encode.Value )
encode attr =
    case attr of
        Height val ->
            ( "height", Encode.int val )

        Width val ->
            ( "width", Encode.int val )

        Alt val ->
            ( "alt", Encode.string val )


{-| Weirdly, Quill seems to want us to pass it an int for
height and width, but it returns us a string.
-}
decodeStringToInt : Decoder Int
decodeStringToInt =
    Decode.string
        |> Decode.andThen
            (\string ->
                case String.toInt string of
                    Just int ->
                        Decode.succeed int

                    Nothing ->
                        Decode.fail "Invalid integer."
            )
