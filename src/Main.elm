module Main exposing (main)

import Browser
import Html as Html exposing (Html)
import Html.Events as Events
import Quill
import Quill.Delta as Delta exposing (Delta)
import Quill.Range as Range exposing (Range)



-- MODEL


type alias Model =
    { selection : Range
    , delta : Delta
    }


init : Model
init =
    { selection = Range.init
    , delta = Delta.init
    }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Quill.view
            { placeholder = "Try me!"
            , content = model.delta
            , selection = model.selection
            , onChange = HandleChange
            }
        , Html.pre []
            [ Html.text (Debug.toString model.selection)
            ]
        , Html.pre []
            [ Html.text (Debug.toString model.delta)
            ]
        , Html.button [ Events.onClick Highlight ]
            [ Html.text "End" ]
        ]



-- UPDATE


type Msg
    = HandleChange Quill.Change
    | Highlight


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleChange { selection, delta } ->
            { model | selection = selection, delta = delta }

        Highlight ->
            { model | selection = { index = model.selection.index, length = 10 } }



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
