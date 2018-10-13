module Main exposing (main)

import Browser
import Html as Html exposing (Html)
import Html.Events as Events
import Quill
import Quill.Attribute as Attribute exposing (Attribute)
import Quill.Attribute.Image as Image
import Quill.Delta as Delta exposing (Delta)
import Quill.Range as Range exposing (Range)



-- MODEL


type alias Model =
    { selection : Range
    , delta : Delta (Attribute Never)
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
            { formats =
                Quill.only
                    [ Quill.image
                    , Quill.bold
                    , Quill.italic
                    , Quill.underline
                    , Quill.list
                    , Quill.background
                    ]
            , placeholder = "Try me!"
            , theme = Just "snow"
            , content = model.delta
            , selection = model.selection
            , onChange = HandleChange
            }
        , Html.pre []
            [ Html.text (Range.debug model.selection)
            ]
        , Html.pre []
            [ Html.text (Debug.toString model.delta)
            ]
        , Html.button [ Events.onClick Highlight ]
            [ Html.text "Highlight Words" ]
        , Html.button [ Events.onClick AddCat ]
            [ Html.text "Internet Points" ]
        ]



-- UPDATE


type Msg
    = HandleChange (Quill.Change (Attribute Never))
    | Highlight
    | AddCat


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleChange { selection, delta } ->
            { model | selection = selection, delta = delta }

        Highlight ->
            { model | delta = highLightWords model.delta }

        AddCat ->
            let
                imageInsert =
                    Delta.Insert <|
                        Delta.Image "https://source.unsplash.com/random?cat"
                            [ Image.Alt "A cute cat."
                            , Image.Height 300
                            ]
            in
            { model | delta = Delta.cons imageInsert model.delta }


{-| This logic is very simple, but it gives an idea of what we can do!
-}
highLightWords : Delta (Attribute Never) -> Delta (Attribute Never)
highLightWords =
    Delta.toString
        >> String.words
        >> List.map
            (\word ->
                Delta.Insert <|
                    Delta.Text word [ Attribute.Background "#ffff00" ]
            )
        >> List.intersperse (Delta.Insert (Delta.Text " " []))
        >> Delta.fromList



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
