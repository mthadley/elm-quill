module Quill exposing
    ( AllowedFormat
    , Change
    , Config
    , Format
    , all
    , background
    , bold
    , custom
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
    , onChange : Change (Attribute Never) -> msg
    , content : Delta (Attribute Never)
    , selection : Range
    , theme : Maybe String
    }


type alias CustomConfig msg attr =
    { formats : AllowedFormat
    , placeholder : String
    , onChange : Change attr -> msg
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


view : Config msg -> Html msg
view config =
    viewCustom
        { formats = config.formats
        , placeholder = config.placeholder
        , onChange = config.onChange
        , content = config.content
        , selection = config.selection
        , theme = config.theme
        , attrDecoder = \_ -> Decode.fail "No custom attributes."
        , attrEncoder = \_ -> ( "", Encode.null )
        }


viewCustom : CustomConfig msg attr -> Html msg
viewCustom config =
    Html.node "elm-quill"
        [ property "formats" (encodeFormat config.formats)
        , property "placeholder" (Encode.string config.placeholder)
        , property "theme" (encodeMaybe Encode.string config.theme)
        , property "content" (Delta.encode config.attrEncoder config.content)
        , property "selection" (Range.encode config.selection)
        , config.attrDecoder
            |> decode
            |> Decode.map config.onChange
            |> on "change"
        ]
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
