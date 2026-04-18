module ControlTestHelper exposing (dummyInstance, dummySetter, lookup, run)

import Component.Internal as Internal exposing (Control(..), ControlI_, Library(..), Lookup)
import Component.Ref as Ref
import Component.Type exposing (Type)
import Dict


{-| Run a control with no library context, returning the ControlI\_ record.
Sufficient for primitives that don't use the library (string, int, etc).
-}
run : Control e t i a -> ControlI_ e t i i a
run (Control f) =
    f emptyLibrary |> Ref.fromTop


{-| Build a Lookup from a list of (Ref, Type) pairs — i.e. from toType output.
-}
lookup : List ( Ref.Ref, Type t ) -> Lookup t
lookup pairs ref =
    Dict.get (Ref.toString ref)
        (Dict.fromList (List.map (\( r, v ) -> ( Ref.toString r, v )) pairs))


emptyLibrary : Library e t
emptyLibrary =
    Library "" { index = [], groups = [], lookupDef = \_ -> Nothing }


{-| A sentinel ComponentInstance for tests that only exercise update functions
ignoring the instance.
-}
dummyInstance : Internal.ComponentInstance
dummyInstance =
    Internal.ComponentInstance (Internal.ComponentRef "") (Ref.fromTop Ref.take)


{-| A no-op setter for tests that don't exercise the setter argument of
`withUpdate`.
-}
dummySetter : state -> Internal.Update t
dummySetter _ =
    Internal.Update dummyInstance []
