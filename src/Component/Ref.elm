module Component.Ref exposing
    ( Ref
    , from
    , fromTop
    , init
    , nested
    , take
    , toString
    , withNestedRef
    )

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


{-| Run inner starting from Ref.init. This means that the rest of the
application need not use State.finalValue.
-}
fromTop : State Ref a -> a
fromTop =
    State.finalValue init


nested : State Ref a -> State Ref a
nested inner =
    take |> State.andThen (\ref -> State.state (from ref inner))


withNestedRef : (Ref -> b) -> State Ref b
withNestedRef inner =
    take |> State.map (\ref -> inner (nest ref))


{-| Get a string representation of Ref. Useful for Dicts, Html identifiers.
-}
toString : Ref -> String
toString (Ref first rest) =
    List.foldl (\i s -> String.fromInt i ++ "." ++ s) (String.fromInt first) rest
