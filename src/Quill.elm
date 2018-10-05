module Quill exposing (Change, Config, view)

import Html as Html exposing (Html)
import Html.Attributes exposing (property)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Quill.Delta as Delta exposing (Delta)
import Quill.Range as Range exposing (Range)


type alias Config msg =
    { placeholder : String
    , onChange : Change -> msg
    , content : Delta
    , selection : Range
    }


view : Config msg -> Html msg
view config =
    Html.node "elm-quill"
        [ property "placeholder" (Encode.string config.placeholder)
        , property "content" (Delta.encode config.content)
        , property "selection" (Range.encode config.selection)
        , on "change" (Decode.map config.onChange decode)
        ]
        []


type alias Change =
    { selection : Range
    , delta : Delta
    }


decode : Decoder Change
decode =
    Decode.map2 Change
        (Decode.field "range" Range.decode)
        (Decode.field "delta" Delta.decode)
        |> Decode.at [ "detail" ]
