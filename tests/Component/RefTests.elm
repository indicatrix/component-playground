module Component.RefTests exposing (suite)

import Component.Ref as Ref exposing (Ref)
import Expect
import State exposing (State)
import Test exposing (Test)


suite : Test
suite =
    let
        test : List String -> State Ref (List Ref) -> Test
        test expected ref =
            Test.test (String.join ", " expected) <|
                \_ ->
                    Expect.equal expected
                        (List.map Ref.toString (State.finalValue Ref.init ref))
    in
    Test.describe "Component.Ref"
        [ Ref.take |> State.map List.singleton |> test [ "0" ]
        , takeStates 3 |> test [ "0", "1", "2" ]
        , Ref.take
            |> State.map
                (\r -> ( r, Ref.from r (takeStates 2) ))
            |> State.andThen
                (\( r1, r2 ) ->
                    takeStates 2
                        |> State.map (\r3 -> r1 :: r2 ++ r3)
                )
            |> test [ "0", "0.0", "0.1", "1", "2" ]
        , Ref.take
            |> State.andThen
                (\r ->
                    Ref.nested (Ref.nested (takeStates 2))
                        |> State.map (\rs -> r :: rs)
                )
            |> test [ "0", "1.0.0", "1.0.1" ]
        ]


takeStates : Int -> State Ref (List Ref)
takeStates i =
    State.traverse (\_ -> Ref.take) (List.repeat i ())
