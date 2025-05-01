module Component.Ref exposing (Ref, from, init, nested, take, toString)

import State exposing (State)


{-| An opaque reference type that can be converted to a string.
-}
type Ref
    = Ref Int (List Int)


{-| Get the starting ref
-}
init : Ref
init =
    Ref 0 []


{-| Get a new state ref.
-}
take : State Ref Ref
take =
    State.advance <|
        \((Ref x xs) as ref) ->
            let
                next =
                    Ref (x + 1) xs
            in
            ( ref, next )


{-| Nest a reference
-}
nest : Ref -> Ref
nest (Ref x xs) =
    Ref 0 (x :: xs)


{-| Run inner starting at a nested ref from the current state
-}
from : Ref -> State Ref a -> a
from ref =
    State.finalValue (nest ref)


nested : State Ref a -> State Ref a
nested inner =
    take |> State.andThen (\ref -> State.state (from ref inner))


{-| Get a string representation of Ref. Useful for Dicts, Html identifiers.
-}
toString : Ref -> String
toString (Ref first rest) =
    List.foldl (\i s -> String.fromInt i ++ "." ++ s) (String.fromInt first) rest
