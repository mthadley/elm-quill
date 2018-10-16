module Main exposing (main)

import Browser
import Css exposing (hex, px)
import Css.Global exposing (class, descendants, typeSelector)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Quill
import Quill.Attribute as Attribute exposing (Attribute)
import Quill.Attribute.Image as Image
import Quill.Delta as Delta exposing (Delta)
import Quill.Range as Range exposing (Range)



-- MODEL


type alias Model =
    { selection : Range
    , highlighting : Bool
    , delta : Delta CustomAttr
    }


init : Model
init =
    { selection = Range.init
    , highlighting = False
    , delta = Delta.fromString frankenstein
    }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ css (highlightStyles model.highlighting) ]
        [ Quill.viewCustom
            { formats =
                Quill.only
                    [ Quill.image
                    , Quill.bold
                    , Quill.italic
                    , Quill.underline
                    , Quill.list
                    , Quill.background
                    , Quill.custom "highlight"
                    ]
            , placeholder = "Try me!"
            , theme = Just "snow"
            , readOnly = model.highlighting
            , content = model.delta
            , selection = model.selection
            , onChange = HandleChange
            , attrDecoder = decodeCustomAttr
            , attrEncoder = encodeCustomAttr
            }
            |> Html.fromUnstyled
        , Html.pre []
            [ Html.text (Range.debug model.selection)
            ]
        , Html.pre []
            [ Html.text (Debug.toString model.delta)
            ]
        , Html.button [ Events.onClick ToggleHighlighting ]
            [ Html.text <|
                if model.highlighting then
                    "Stop Highlighting"

                else
                    "Highlight Words"
            ]
        , Html.button [ Events.onClick AddCat ]
            [ Html.text "Internet Points" ]
        ]



-- UPDATE


type Msg
    = HandleChange (Quill.Change CustomAttr)
    | ToggleHighlighting
    | AddCat


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleChange { selection, delta } ->
            if model.highlighting then
                { model
                    | delta =
                        if selection.length > 0 then
                            Delta.format
                                (Attribute.Custom Highlight)
                                selection
                                delta

                        else
                            delta
                    , selection = { selection | length = 0 }
                }

            else
                { model | selection = selection, delta = delta }

        ToggleHighlighting ->
            { model | highlighting = not model.highlighting }

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



-- CUSTOM ATTR


type CustomAttr
    = Highlight


decodeCustomAttr : String -> Decoder CustomAttr
decodeCustomAttr name =
    case name of
        "highlight" ->
            Decode.succeed Highlight

        invalid ->
            Decode.fail <| "Invalid custom attribute: \"" ++ invalid ++ "\""


encodeCustomAttr : CustomAttr -> ( String, Encode.Value )
encodeCustomAttr customAttr =
    case customAttr of
        Highlight ->
            ( "highlight", Encode.bool True )



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = Html.toUnstyled << view
        , update = update
        }



-- CSS


highlightStyles : Bool -> List Css.Style
highlightStyles highlighting =
    [ descendants
        [ class "highlight"
            [ Css.backgroundColor (hex "FF0")
            , Css.color Css.inherit
            , Css.fontWeight Css.inherit
            , Css.property "transition" "background-color 0.4s ease"
            , Css.batch <|
                if highlighting then
                    [ Css.cursor Css.pointer
                    , Css.hover
                        [ Css.backgroundColor (hex "DDA")
                        ]
                    ]

                else
                    []
            ]
        ]
    ]



-- ESSAY


frankenstein : String
frankenstein =
    """    These reflections have dispelled the agitation with which I began my letter, and I feel my heart glow with an enthusiasm which elevates me to heaven, for nothing contributes so much to tranquillise the mind as a steady purpose—a point on which the soul may fix its intellectual eye. This expedition has been the favourite dream of my early years. I have read with ardour the accounts of the various voyages which have been made in the prospect of arriving at the North Pacific Ocean through the seas which surround the pole. You may remember that a history of all the voyages made for purposes of discovery composed the whole of our good Uncle Thomas’ library. My education was neglected, yet I was passionately fond of reading. These volumes were my study day and night, and my familiarity with them increased that regret which I had felt, as a child, on learning that my father’s dying injunction had forbidden my uncle to allow me to embark in a seafaring life.
    These visions faded when I perused, for the first time, those poets whose effusions entranced my soul and lifted it to heaven. I also became a poet and for one year lived in a paradise of my own creation; I imagined that I also might obtain a niche in the temple where the names of Homer and Shakespeare are consecrated. You are well acquainted with my failure and how heavily I bore the disappointment. But just at that time I inherited the fortune of my cousin, and my thoughts were turned into the channel of their earlier bent.
    Six years have passed since I resolved on my present undertaking. I can, even now, remember the hour from which I dedicated myself to this great enterprise. I commenced by inuring my body to hardship. I accompanied the whale-fishers on several expeditions to the North Sea; I voluntarily endured cold, famine, thirst, and want of sleep; I often worked harder than the common sailors during the day and devoted my nights to the study of mathematics, the theory of medicine, and those branches of physical science from which a naval adventurer might derive the greatest practical advantage. Twice I actually hired myself as an under-mate in a Greenland whaler, and acquitted myself to admiration. I must own I felt a little proud when my captain offered me the second dignity in the vessel and entreated me to remain with the greatest earnestness, so valuable did he consider my services.
    """
