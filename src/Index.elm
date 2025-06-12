module Index exposing (main)

import Component
import Component.Application
import Component.UI as UI
import Html
import Html.Events
import Task
import Time


textFieldPreview : Component.Preview t (Component.Msg t msg) (Html.Html (Component.Msg t msg))
textFieldPreview =
    Component.preview "text-field"
        { name = "Text field" }
        (\s msg l i err ->
            let
                e =
                    if err == "" then
                        Nothing

                    else
                        Just err
            in
            UI.textField { msg = msg, label = l, id = i, value = s, error = e }
        )
        |> Component.withState_ "Value" Component.string
        |> Component.withControl "Label" Component.string "Label"
        |> Component.withUnlabelled_ Component.identifier
        |> Component.withControl "Error" Component.string ""


dropdownInputPreview : Component.Preview t (Component.Msg t msg) (Html.Html (Component.Msg t msg))
dropdownInputPreview =
    Component.preview "dropdown-input"
        { name = "Simple Dropdown Input" }
        (\label selected msg options i ->
            UI.select
                { id = i
                , label = label
                , options = options
                , value = selected
                , msg = msg
                }
        )
        |> Component.withControl "Label" Component.string "Label"
        |> Component.withState "Value" Component.string "2"
        |> Component.withControl "Options"
            (Component.build (\label value -> { label = label, value = value })
                |> Component.addVia .label "Label" Component.string
                |> Component.addVia .value "Value" Component.string
                |> Component.finish_
                |> Component.list
            )
            [ { label = "One", value = "1" }
            , { label = "Two", value = "2" }
            , { label = "Three", value = "3" }
            ]
        |> Component.withUnlabelled_ Component.identifier


type Msg
    = Inc
    | Tick Time.Posix


type alias Model =
    { counter : Int
    , lastTick : Maybe Time.Posix
    }


main : Component.Application.ComponentPlayground Model () Msg
main =
    let
        previews : List (Component.Preview t (Component.Msg t Msg) (Html.Html (Component.Msg t Msg)))
        previews =
            [ textFieldPreview
            , dropdownInputPreview
            , Component.preview "test-1"
                { name = "Test 1" }
                (\a b c ->
                    UI.vStack []
                        [ Html.div [] [ UI.text [] [ Html.text a ] ]
                        , Html.div [] [ UI.text [] [ Html.text b ] ]
                        , Html.div [] [ UI.text [] [ Html.text c ] ]
                        , Html.div [] [ UI.button [ Html.Events.onClick (Component.componentMsg Inc) ] [ Html.text "Test button" ] ]
                        ]
                )
                |> Component.withUnlabelled_ Component.identifier
                |> Component.withUnlabelled_ Component.identifier
                |> Component.withUnlabelled_ Component.identifier
            , Component.preview "test-2"
                { name = "Test 2" }
                (\a b ->
                    UI.vStack []
                        [ Html.div [] [ UI.text [] [ Html.text a ] ]
                        , Html.div [] [ UI.text [] [ Html.text b ] ]
                        ]
                )
                |> Component.withUnlabelled_ Component.identifier
                |> Component.withUnlabelled_ Component.identifier
            , Component.preview "int-input"
                { name = "Int Input" }
                (\a ->
                    Html.div [] [ Html.text <| "Int value: " ++ String.fromInt a ]
                )
                |> Component.withControl "Int Value" Component.int 5
            , Component.preview "float-input"
                { name = "Float Input" }
                (\a ->
                    Html.div [] [ Html.text <| "Float value: " ++ String.fromFloat a ]
                )
                |> Component.withControl "Float Value" Component.float 0.5
            , Component.preview "list-test"
                { name = "List test" }
                (\ll ->
                    UI.text [] [ Html.text <| String.join ", " ll ]
                )
                |> Component.withControl "Contents"
                    (Component.list Component.string)
                    [ "One", "Two", "Three" ]
            , Component.preview "combo-element"
                { name = "Combination Element" }
                (\title inner innerList ->
                    UI.vStack [ UI.style "gap" "8px" ]
                        ([ UI.text [] [ Html.text title ]
                         , inner
                         ]
                            ++ innerList
                        )
                )
                |> Component.withControl "Title" Component.string "Title"
                |> Component.withPreview_ "Element" Component.previewBlock
                |> Component.withPreview "Element list"
                    (Component.list2 Component.previewBlock)
                    [ Component.fromPreview textFieldPreview, Component.fromPreview dropdownInputPreview ]
            ]

        update : Msg -> Model -> ( Model, Cmd Msg )
        update msg ({ counter } as model) =
            case msg of
                Inc ->
                    ( { model | counter = counter + 1 }
                    , Task.perform Tick Time.now
                    )

                Tick t ->
                    ( { model | lastTick = Just t }, Cmd.none )

        init : Model
        init =
            { counter = 0, lastTick = Nothing }

        subscriptions : Sub Msg
        subscriptions =
            Time.every 10000 Tick
    in
    Component.Application.playground init update subscriptions previews
