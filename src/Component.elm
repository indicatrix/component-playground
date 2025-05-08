module Component exposing
    ( Block
    , Builder
    , Library
    , Lookup
    , Msg
    , Preview
    , addVia
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
    , withControl_
    , withMsg
    , withState
    , withState_
    , withSubcomponent
    , withUnlabelled
    , withUnlabelled_
    )

import Component.Block as Block
import Component.Preview as Preview
import Html exposing (Html)


type alias Library t msg =
    Preview.Library t msg


type alias Preview t msg a =
    Preview.Preview t msg a


type alias Block t a =
    Block.Block t a


type alias Lookup t =
    Block.Lookup t


type alias Msg t msg =
    Preview.Msg t msg


type alias Builder t r a =
    Block.Builder t r a


map : (a -> b) -> Preview t msg a -> Preview t msg b
map =
    Preview.map


preview : String -> { name : String } -> a -> Preview t msg a
preview =
    Preview.preview


subcomponent : Library t msg -> String -> Block t (Html msg)
subcomponent =
    Preview.subcomponent


withControl : String -> (String -> Block t a) -> a -> Preview t msg (a -> b) -> Preview t msg b
withControl label block default =
    Preview.withControl label (\l -> Block.withDefault default (block l))


withControl_ : String -> (String -> Block t a) -> Preview t msg (a -> b) -> Preview t msg b
withControl_ =
    Preview.withControl


withMsg : Preview t (Msg t m) ((m -> Msg t m) -> a) -> Preview t (Msg t m) a
withMsg =
    Preview.withMsg


withState : String -> (String -> Block t a) -> a -> Preview t (Msg t msg) (a -> (a -> Msg t msg) -> c) -> Preview t (Msg t msg) c
withState label block default =
    Preview.withState label (\l -> Block.withDefault default (block l))


withState_ : String -> (String -> Block t a) -> Preview t (Msg t msg) (a -> (a -> Msg t msg) -> c) -> Preview t (Msg t msg) c
withState_ =
    Preview.withState


withSubcomponent : String -> (Library t msg -> String -> Block t b) -> Preview t msg (b -> a) -> Preview t msg a
withSubcomponent =
    Preview.withSubcomponent


withUnlabelled : Block t a -> a -> Preview t msg (a -> b) -> Preview t msg b
withUnlabelled block default =
    Preview.withUnlabelled (Block.withDefault default block)


withUnlabelled_ : Block t a -> Preview t msg (a -> b) -> Preview t msg b
withUnlabelled_ =
    Preview.withUnlabelled


addVia : (r -> a) -> String -> (String -> Block t a) -> Builder t r (a -> b) -> Builder t r b
addVia =
    Block.addVia


build : a -> Builder t r a
build =
    Block.build


finish : Builder t a a -> String -> Block t a
finish =
    Block.finish


identifier : Block t String
identifier =
    Block.identifier


list : (String -> Block t a) -> String -> Block t (List a)
list =
    Block.list


list2 : (g -> String -> Block t a) -> g -> String -> Block t (List a)
list2 =
    Block.list2


string : String -> Block t String
string =
    Block.string


oneOf : ( a, String ) -> List ( a, String ) -> String -> Block t a
oneOf =
    Block.oneOf
