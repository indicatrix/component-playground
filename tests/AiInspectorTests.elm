module AiInspectorTests exposing (suite)

import Component.Application.AiInspector as AI
import Dict
import Expect
import Test exposing (Test, describe, test)


ctx : AI.Context
ctx =
    { componentId = Just "comp-a"
    , componentName = "Component A"
    , route = "comp-a"
    , sourceFile = Nothing
    }


sampleElement : AI.SelectedElement
sampleElement =
    { id = "sample-title-h1"
    , label = "Title (h1)"
    , subtitle = Just "Review account requirements"
    , elementType = "Heading"
    , tagName = "h1"
    , role = Just "heading"
    , textContent = Just "Review account requirements"
    , componentName = "Component A"
    , componentId = Just "comp-a"
    , route = "comp-a"
    , sourceFile = Nothing
    , sourceSymbol = Nothing
    , selector = Just "h1.auth-title"
    , classNames = [ "auth-title" ]
    , dataAttributes = []
    , tokens = []
    , computedStyles = []
    , bounds = Nothing
    }


{-| Apply a sequence of messages from `init`.
-}
run : List AI.Msg -> AI.Model
run msgs =
    List.foldl (\msg model -> AI.update ctx msg model) AI.init msgs


{-| A model with an element selected under comp-a (no work yet).
-}
selected : AI.Model
selected =
    run [ AI.SelectionCaptured sampleElement ]


{-| A model with one in-progress work item under comp-a, created via a prompt.
Its id is "work-1" (the first allocated id).
-}
withWork : AI.Model
withWork =
    run [ AI.SelectionCaptured sampleElement, AI.ChatInputChanged "Do a thing", AI.SubmitPrompt ]


suite : Test
suite =
    describe "AiInspector"
        [ describe "selection"
            [ test "init is the default, empty state" <|
                \_ ->
                    Expect.equal ( AI.init.mode, AI.init.selected ) ( AI.Default, Nothing )
            , test "StartSelecting enters Selecting with no selection" <|
                \_ ->
                    let
                        m =
                            run [ AI.StartSelecting ]
                    in
                    Expect.equal ( m.mode, m.selected ) ( AI.Selecting, Nothing )
            , test "SelectionCaptured opens Selected" <|
                \_ ->
                    Expect.equal selected.mode AI.Selected
            , test "a selection is present after SelectionCaptured" <|
                \_ ->
                    Expect.notEqual selected.selected Nothing
            , test "ClearSelection returns to Default and clears the selection" <|
                \_ ->
                    let
                        m =
                            run [ AI.SelectionCaptured sampleElement, AI.ClearSelection ]
                    in
                    Expect.equal ( m.mode, m.selected ) ( AI.Default, Nothing )
            ]
        , describe "navigation preserves state (clarification #4)"
            [ test "Token editor tab is remembered through Work history and Back" <|
                \_ ->
                    let
                        m =
                            run
                                [ AI.SelectionCaptured sampleElement
                                , AI.SwitchTab AI.TokenEditor
                                , AI.OpenWorkHistory
                                , AI.Back
                                ]
                    in
                    Expect.equal ( m.mode, m.tab ) ( AI.Selected, AI.TokenEditor )
            , test "Change details Back returns to Work history, then to Selected" <|
                \_ ->
                    let
                        afterDetails =
                            run
                                [ AI.SelectionCaptured sampleElement
                                , AI.ChatInputChanged "Do a thing"
                                , AI.SubmitPrompt
                                , AI.OpenWorkHistory
                                , AI.OpenChangeDetails "work-1"
                                , AI.Back
                                ]

                        afterHistory =
                            AI.update ctx AI.Back afterDetails
                    in
                    Expect.equal ( afterDetails.mode, afterHistory.mode )
                        ( AI.WorkHistory, AI.Selected )
            ]
        , describe "navigation reset (component-scoped view state)"
            [ test "resetForNavigation clears selection/mode but keeps work" <|
                \_ ->
                    let
                        m =
                            AI.resetForNavigation withWork
                    in
                    Expect.equal
                        ( m.mode, m.selected, List.isEmpty (AI.historyFor ctx m) )
                        ( AI.Default, Nothing, False )
            ]
        , describe "history is component-scoped (clarification #5)"
            [ test "work is visible for its own component" <|
                \_ ->
                    Expect.equal (List.isEmpty (AI.historyFor ctx withWork)) False
            , test "work is not visible for a different component" <|
                \_ ->
                    let
                        otherCtx =
                            { ctx | componentId = Just "comp-b", route = "comp-b" }
                    in
                    Expect.equal (AI.historyFor otherCtx withWork) []
            ]
        , describe "lifecycle"
            [ test "a new work item is in progress, so work is active" <|
                \_ ->
                    Expect.equal (AI.hasActiveWork withWork) True
            , test "enough ticks settle all work (no active work remains)" <|
                \_ ->
                    let
                        -- 4 phases * 1200ms, plus slack; 60 ticks of 500ms.
                        ticked =
                            List.foldl (\_ m -> AI.update ctx (AI.Tick 500) m)
                                withWork
                                (List.range 1 60)
                    in
                    Expect.equal (AI.hasActiveWork ticked) False
            , test "a 'fail' prompt settles to a failed item" <|
                \_ ->
                    let
                        failed =
                            List.foldl (\_ m -> AI.update ctx (AI.Tick 500) m)
                                (run
                                    [ AI.SelectionCaptured sampleElement
                                    , AI.ChatInputChanged "please fail this on purpose"
                                    , AI.SubmitPrompt
                                    ]
                                )
                                (List.range 1 60)
                    in
                    Expect.equal
                        (AI.historyFor ctx failed
                            |> List.filter (\i -> i.status == AI.Failed)
                            |> List.isEmpty
                            |> not
                        )
                        True
            ]
        , describe "token dropdown"
            [ test "OpenDropdown then TokenChosen records a draft and closes" <|
                \_ ->
                    let
                        m =
                            run
                                [ AI.SelectionCaptured sampleElement
                                , AI.SwitchTab AI.TokenEditor
                                , AI.OpenDropdown "typography"
                                , AI.TokenChosen "typography" "text-heading-1"
                                ]
                    in
                    Expect.equal
                        ( m.openDropdown, Dict.get "typography" m.tokenDrafts )
                        ( Nothing, Just "text-heading-1" )
            , test "switching tab closes any open dropdown" <|
                \_ ->
                    let
                        m =
                            run
                                [ AI.SelectionCaptured sampleElement
                                , AI.SwitchTab AI.TokenEditor
                                , AI.OpenDropdown "typography"
                                , AI.SwitchTab AI.AgentChat
                                ]
                    in
                    Expect.equal m.openDropdown Nothing
            ]
        , describe "work creation (slice 8)"
            [ test "submitting a prompt adds one work item and clears the input" <|
                \_ ->
                    Expect.equal
                        ( List.length (AI.historyFor ctx withWork), withWork.chatInput )
                        ( 1, "" )
            , test "an empty prompt creates no work" <|
                \_ ->
                    let
                        m =
                            run
                                [ AI.SelectionCaptured sampleElement
                                , AI.ChatInputChanged "   "
                                , AI.SubmitPrompt
                                ]
                    in
                    Expect.equal (List.length (AI.historyFor ctx m)) 0
            ]
        ]
