module Quill.Attribute exposing (AttrDecoder, AttrEncoder, Attribute(..), decode, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias AttrDecoder attr =
    ( String, Encode.Value ) -> Maybe attr


type alias AttrEncoder attr =
    attr -> ( String, Encode.Value )


{-| The built in Quill attributes, like "bold" and "italics".
-}
type Attribute
    = Bold
    | Italic
    | Link String


decode : AttrDecoder Attribute
decode pair =
    case pair of
        ( "bold", _ ) ->
            Just Bold

        ( "italic", _ ) ->
            Just Italic

        ( "link", value ) ->
            value
                |> Decode.decodeValue Decode.string
                |> Result.toMaybe
                |> Maybe.map Link

        _ ->
            Nothing


encode : AttrEncoder Attribute
encode attr =
    case attr of
        Bold ->
            ( "bold", Encode.bool True )

        Italic ->
            ( "italic", Encode.bool True )

        Link url ->
            ( "link", Encode.string url )
