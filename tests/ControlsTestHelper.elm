module ControlsTestHelper exposing (lookup, run)

import Component.Internal exposing (Controls(..), ControlsI_, Library(..), Lookup)
import Component.Ref as Ref
import Component.Type exposing (Type)
import Dict


{-| Run controls with no library context, returning the ControlsI\_ record.
Sufficient for primitives that don't use the library (string, int, etc).
-}
run : Controls e t i a -> ControlsI_ e t i i a
run (Controls f) =
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
