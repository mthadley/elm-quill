module Quill exposing
    ( AllowedFormat
    , Change
    , Config
    , Format
    , all
    , background
    , bold
    , custom
    , image
    , italic
    , link
    , list
    , only
    , underline
    , view
    , viewCustom
    )

import Html as Html exposing (Html)
import Html.Attributes exposing (property)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Quill.Attribute as Attribute exposing (Attribute)
import Quill.Delta as Delta exposing (Delta)
import Quill.Range as Range exposing (Range)


type alias Config msg =
    { formats : AllowedFormat
    , placeholder : String
    , onChange : Change Never -> msg
    , readOnly : Bool
    , content : Delta Never
    , selection : Range
    , theme : Maybe String
    }


type alias CustomConfig msg attr =
    { formats : AllowedFormat
    , placeholder : String
    , onChange : Change attr -> msg
    , readOnly : Bool
    , content : Delta attr
    , selection : Range
    , theme : Maybe String
    , attrDecoder : String -> Decoder attr
    , attrEncoder : attr -> ( String, Encode.Value )
    }


type alias Change attr =
    { selection : Range
    , delta : Delta attr
    }


type AllowedFormat
    = All
    | Only (List Format)


type Format
    = Format String



-- VIEW


view : List (Html.Attribute msg) -> Config msg -> Html msg
view attributes config =
    viewCustom
        attributes
        { formats = config.formats
        , placeholder = config.placeholder
        , onChange = config.onChange
        , readOnly = config.readOnly
        , content = config.content
        , selection = config.selection
        , theme = config.theme
        , attrDecoder = \_ -> Decode.fail "No custom attributes."
        , attrEncoder = \_ -> ( "", Encode.null )
        }


viewCustom : List (Html.Attribute msg) -> CustomConfig msg attr -> Html msg
viewCustom attributes config =
    Html.node "elm-quill"
        (attributes
            ++ [ property "formats" (encodeFormat config.formats)
               , property "placeholder" (Encode.string config.placeholder)
               , property "theme" (encodeMaybe Encode.string config.theme)
               , property "readOnly" (Encode.bool config.readOnly)
               , property "content" (Delta.encode config.attrEncoder config.content)
               , property "selection" (Range.encode config.selection)
               , config.attrDecoder
                    |> decode
                    |> Decode.map config.onChange
                    |> on "change"
               ]
        )
        []



-- FORMATS


all : AllowedFormat
all =
    All


only : List Format -> AllowedFormat
only =
    Only


bold : Format
bold =
    Format "bold"


italic : Format
italic =
    Format "italic"


underline : Format
underline =
    Format "underline"


link : Format
link =
    Format "link"


list : Format
list =
    Format "list"


background : Format
background =
    Format "background"


image : Format
image =
    Format "image"


custom : String -> Format
custom =
    Format



-- CODECS


decode : (String -> Decoder attr) -> Decoder (Change attr)
decode attrDecoder =
    Decode.map2 Change
        (Decode.field "range" Range.decode)
        (Decode.field "delta" <| Delta.decode attrDecoder)
        |> Decode.at [ "detail" ]


encodeFormat : AllowedFormat -> Encode.Value
encodeFormat allowedFormat =
    case allowedFormat of
        All ->
            Encode.null

        Only formats ->
            Encode.list (\(Format name) -> Encode.string name) formats


encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe encoder =
    Maybe.map encoder >> Maybe.withDefault Encode.null
