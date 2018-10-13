module Quill.Attribute exposing (Attribute(..), ListStyle(..), decode, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Attribute attr
    = Bold
    | Italic
    | Underline
    | Link String
    | List ListStyle
    | Background String
    | Custom attr


type ListStyle
    = Bullet
    | Ordered


decode : (String -> Decoder attr) -> String -> Decoder (Attribute attr)
decode decodeAttr pair =
    case pair of
        "bold" ->
            Decode.succeed Bold

        "italic" ->
            Decode.succeed Italic

        "underline" ->
            Decode.succeed Underline

        "link" ->
            Decode.map Link Decode.string

        "list" ->
            Decode.string
                |> Decode.andThen
                    (\style ->
                        case style of
                            "bullet" ->
                                Decode.succeed Bullet

                            "ordered" ->
                                Decode.succeed Ordered

                            listStyle ->
                                Decode.fail <| "Unsupported list style \"" ++ listStyle ++ "\""
                    )
                |> Decode.map List

        "background" ->
            Decode.map Background Decode.string

        name ->
            Decode.map Custom (decodeAttr name)


encode : (attr -> ( String, Encode.Value )) -> Attribute attr -> ( String, Encode.Value )
encode encodeCustom attr =
    case attr of
        Bold ->
            ( "bold", Encode.bool True )

        Italic ->
            ( "italic", Encode.bool True )

        Underline ->
            ( "underline", Encode.bool True )

        Link url ->
            ( "link", Encode.string url )

        List style ->
            ( "list"
            , Encode.string <|
                case style of
                    Bullet ->
                        "bullet"

                    Ordered ->
                        "ordered"
            )

        Background color ->
            ( "background", Encode.string color )

        Custom customAttr ->
            encodeCustom customAttr
