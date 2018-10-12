module Quill exposing (Change, Config, view, viewCustom)

import Html as Html exposing (Html)
import Html.Attributes exposing (property)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Quill.Attribute as Attribute exposing (Attribute)
import Quill.Delta as Delta exposing (Delta)
import Quill.Range as Range exposing (Range)


type alias Config msg =
    { placeholder : String
    , onChange : Change Attribute -> msg
    , content : Delta Attribute
    , selection : Range
    }


type alias CustomConfig msg attr =
    { placeholder : String
    , onChange : Change attr -> msg
    , content : Delta attr
    , selection : Range
    , attrDecoder : String -> Decoder attr
    , attrEncoder : attr -> ( String, Encode.Value )
    }


view : Config msg -> Html msg
view config =
    viewCustom
        { placeholder = config.placeholder
        , onChange = config.onChange
        , content = config.content
        , selection = config.selection
        , attrDecoder = Attribute.decode
        , attrEncoder = Attribute.encode
        }


viewCustom : CustomConfig msg attr -> Html msg
viewCustom config =
    Html.node "elm-quill"
        [ property "placeholder" (Encode.string config.placeholder)
        , property "content" (Delta.encode config.attrEncoder config.content)
        , property "selection" (Range.encode config.selection)
        , config.attrDecoder
            |> decode
            |> Decode.map config.onChange
            |> on "change"
        ]
        []


type alias Change attr =
    { selection : Range
    , delta : Delta attr
    }


decode : (String -> Decoder attr) -> Decoder (Change attr)
decode attrDecoder =
    Decode.map2 Change
        (Decode.field "range" Range.decode)
        (Decode.field "delta" <| Delta.decode attrDecoder)
        |> Decode.at [ "detail" ]
