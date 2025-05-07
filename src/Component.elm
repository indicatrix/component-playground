module Component exposing
    ( Block
    , Builder
    , Library
    , Lookup
    , Msg
    , Preview
    , add
    , build
    , finish
    , identifier
    , list
    , list2
    , map
    , oneOf
    , preview
    , string
    , subcomponent
    , withControl
    , withMsg
    , withState
    , withSubcomponent
    , withUnlabelled
    )

import Component.Block as Block
import Component.Preview as Preview
import Html exposing (Html)


type alias Library t msg =
    Preview.Library t msg


type alias Preview t msg a =
    Preview.Preview t msg a


type alias Block t x a =
    Block.Block t x a


type alias Lookup t =
    Block.Lookup t


type alias Msg t msg =
    Preview.Msg t msg


type alias Builder a =
    Block.Builder a


map : (a -> b) -> Preview t msg a -> Preview t msg b
map =
    Preview.map


preview : String -> { name : String } -> a -> Preview t msg a
preview =
    Preview.preview


subcomponent : Library t msg -> String -> Block t x (Html msg)
subcomponent =
    Preview.subcomponent


withControl : String -> (String -> Block t x a) -> Preview t msg (a -> b) -> Preview t msg b
withControl =
    Preview.withControl


withMsg : Preview t (Msg t m) ((m -> Msg t m) -> a) -> Preview t (Msg t m) a
withMsg =
    Preview.withMsg


withState : String -> (String -> Block t a a) -> Preview t (Msg t msg) (a -> (a -> Msg t msg) -> c) -> Preview t (Msg t msg) c
withState =
    Preview.withState


withSubcomponent : String -> (Library t msg -> String -> Block t x b) -> Preview t msg (b -> a) -> Preview t msg a
withSubcomponent =
    Preview.withSubcomponent


withUnlabelled : Block t x a -> Preview t msg (a -> b) -> Preview t msg b
withUnlabelled =
    Preview.withUnlabelled


add : String -> (String -> Block t x1 a) -> Builder (Block t x (a -> b)) -> Builder (Block t x b)
add =
    Block.add


build : a -> Builder (Block t x a)
build =
    Block.build


finish : Builder (Block t x a) -> String -> Block t x a
finish =
    Block.finish


identifier : Block t x String
identifier =
    Block.identifier


list : (String -> Block t b a) -> String -> Block t (List b) (List a)
list =
    Block.list


list2 : (g -> String -> Block t b a) -> g -> String -> Block t (List b) (List a)
list2 =
    Block.list2


string : String -> Block t String String
string =
    Block.string


oneOf : ( a, String ) -> List ( a, String ) -> String -> Block t a a
oneOf =
    Block.oneOf
