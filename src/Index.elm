module Index exposing (Model, main)

import Browser
import Html exposing (Html)
import Json.Decode as Decode


type alias Model =
    {}


main : Program Decode.Value Model ()
main =
    Browser.element
        { init = \_ -> ( {}, Cmd.none )
        , view = \_ -> Html.text "Is this working?"
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
