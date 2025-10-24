module Component exposing
    ( Block, BlockI, Builder, Component, ComponentRef, Library, Lookup, Msg, Preview, PreviewGroup, Ref, Type, View
    , group
    , new, withComponent, withComponent_, withControl, withControl_, withMsg, withMsg2, withMsg3, withState, withStateF, withStateF_, withState_, withUnlabelled, withUnlabelledState, withUnlabelledStateF, withUnlabelledStateF_, withUnlabelledState_, withUnlabelled_, withUpdateF, withMsgF, fromPreview, map
    , previewBlock, identifier, list, list2, bool, int, float, string, oneOf, stringEntryBlock, custom
    , addVia, build, finish, finish_
    , toPortalPreview, toPreview
    , toComponentMsg, withDefault
    )

{-| TODO: write a description of the module, and write descriptions for each section of the docs

#Re-exported Aliases

These opaque types are defined and exported from submodules. They are aliased
and exported here so that it is possible to write explicit type signatures.

@docs Block, BlockI, Builder, Component, ComponentRef, Library, Lookup, Msg, Preview, PreviewGroup, Ref, Type, View

#Groups

@docs group

#Constructing Components

@docs new, withComponent, withComponent_, withControl, withControl_, withMsg, withMsg2, withMsg3, withState, withStateF, withStateF_, withState_, withUnlabelled, withUnlabelledState, withUnlabelledStateF, withUnlabelledStateF_, withUnlabelledState_, withUnlabelled_, withUpdateF, withMsgF, fromPreview, map

#Blocks

@docs previewBlock, identifier, list, list2, bool, int, float, string, oneOf, stringEntryBlock, custom

#Building Blocks

@docs addVia, build, finish, finish_

#Constructing Previews

@docs toPortalPreview, toPreview

#Messages

@docs toComponentMsg

-}

import Component.Block as Block
import Component.Component as Component
import Component.Ref as Ref
import Component.Type
import Dict
import Html exposing (Html)



{- Re-export types from submodules -}


type alias Library t msg =
    Component.Library t msg


type alias Component t msg a =
    Component.Component t msg a


type alias Preview t msg =
    Component.Preview t msg


type alias PreviewGroup t msg =
    Component.PreviewGroup t msg


group : String -> List (Preview t msg) -> PreviewGroup t msg
group =
    Component.group


type alias Block t a =
    Block.Block t a


type alias BlockI t i a =
    Block.BlockI t i a


type alias Lookup t =
    Block.Lookup t


type alias Msg t msg =
    Component.Msg t msg


type alias View msg =
    Component.View msg


type alias Builder t i r a =
    Block.Builder t i r a


type alias ComponentRef =
    Component.ComponentRef


type alias Ref =
    Ref.Ref


type alias Type t =
    Component.Type.Type t


toComponentMsg : msg -> Msg t msg
toComponentMsg msg =
    Component.Msg [] msg


map : (a -> b) -> Component t msg a -> Component t msg b
map =
    Component.map


toPreview : { id : String, name : String } -> Component t msg (Html msg) -> Preview t msg
toPreview meta component =
    ( meta, Component.map (\html -> ( html, Dict.empty )) component )


toPortalPreview : { id : String, name : String } -> Component t msg (View msg) -> Preview t msg
toPortalPreview meta component =
    ( meta, component )


new : a -> Component t msg a
new =
    Component.new


previewBlock : Library t msg -> String -> BlockI t ComponentRef (Html msg)
previewBlock =
    Component.previewBlock


fromPreview : Preview t msg -> ComponentRef
fromPreview ( meta, _ ) =
    Component.ComponentRef meta.id


withControl : String -> (String -> Block t a) -> a -> Component t msg (a -> b) -> Component t msg b
withControl label block default =
    Component.withControl label (\l -> Block.withDefault default (block l))


withControl_ : String -> (String -> Block t a) -> Component t msg (a -> b) -> Component t msg b
withControl_ =
    Component.withControl


withMsg : (a -> msg) -> Component t (Msg t msg) ((a -> Msg t msg) -> r) -> Component t (Msg t msg) r
withMsg =
    Component.withMsg


withMsg2 : (a -> b -> msg) -> Component t (Msg t msg) ((a -> b -> Msg t msg) -> r) -> Component t (Msg t msg) r
withMsg2 =
    Component.withMsg2


withMsg3 : (a -> b -> c -> msg) -> Component t (Msg t msg) ((a -> b -> c -> Msg t msg) -> r) -> Component t (Msg t msg) r
withMsg3 =
    Component.withMsg3


withState : String -> (String -> BlockI t i a) -> i -> Component t (Msg t msg) (a -> (i -> Msg t msg) -> y) -> Component t (Msg t msg) y
withState label blockF default =
    withState_ label (\l -> Block.withDefault default (blockF l))


withState_ : String -> (String -> BlockI t i a) -> Component t (Msg t msg) (a -> (i -> Msg t msg) -> y) -> Component t (Msg t msg) y
withState_ label blockF =
    Component.withState (blockF label) (\get set f -> f get set)


withStateF : String -> (String -> BlockI t i a) -> i -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y) -> Component t (Msg t msg) x -> Component t (Msg t msg) y
withStateF label blockF default =
    withStateF_ label (\l -> Block.withDefault default (blockF l))


withStateF_ : String -> (String -> BlockI t i a) -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y) -> Component t (Msg t msg) x -> Component t (Msg t msg) y
withStateF_ label blockF =
    Component.withStateF (blockF label)


withUnlabelledState : BlockI t i a -> i -> Component t (Msg t msg) (a -> (i -> Msg t msg) -> b) -> Component t (Msg t msg) b
withUnlabelledState block default =
    withUnlabelledState_ (Block.withDefault default block)


withUnlabelledState_ : BlockI t i a -> Component t (Msg t msg) (a -> (i -> Msg t msg) -> b) -> Component t (Msg t msg) b
withUnlabelledState_ block =
    Component.withUnlabelledState block (\get set f -> f get set)


withUnlabelledStateF : BlockI t i a -> i -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y) -> Component t (Msg t msg) x -> Component t (Msg t msg) y
withUnlabelledStateF block default =
    withUnlabelledStateF_ (Block.withDefault default block)


withUnlabelledStateF_ : BlockI t i a -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y) -> Component t (Msg t msg) x -> Component t (Msg t msg) y
withUnlabelledStateF_ =
    Component.withStateF


withUpdateF :
    BlockI t i a
    -> (Ref -> a -> ((a -> ( i, msg )) -> Msg t msg) -> x -> y)
    -> Component t (Msg t msg) x
    -> Component t (Msg t msg) y
withUpdateF =
    Component.withUpdateF


withMsgF :
    ((msg -> Msg t msg) -> x -> y)
    -> Component t (Msg t msg) x
    -> Component t (Msg t msg) y
withMsgF =
    Component.withMsgF


withComponent : String -> (Library t msg -> String -> BlockI t i b) -> i -> Component t msg (b -> a) -> Component t msg a
withComponent label block default =
    Component.withComponent label (\lib l -> Block.withDefault default (block lib l))


withComponent_ : String -> (Library t msg -> String -> BlockI t i b) -> Component t msg (b -> a) -> Component t msg a
withComponent_ =
    Component.withComponent


withUnlabelled : BlockI t i a -> i -> Component t msg (a -> b) -> Component t msg b
withUnlabelled block default =
    Component.withUnlabelled (Block.withDefault default block)


withUnlabelled_ : BlockI t i a -> Component t msg (a -> b) -> Component t msg b
withUnlabelled_ =
    Component.withUnlabelled


addVia : (r -> a) -> String -> (String -> BlockI t a a) -> Builder t (a -> b) r (a -> b) -> Builder t b r b
addVia =
    Block.addVia


build : a -> Builder t a r a
build =
    Block.build


finish : (i -> a) -> Builder t i i i -> String -> BlockI t i a
finish f =
    Block.finishI f


finish_ : Builder t a a a -> String -> BlockI t a a
finish_ =
    Block.finishI identity


identifier : BlockI t String String
identifier =
    Block.identifier


list : (String -> BlockI t i a) -> String -> BlockI t (List i) (List a)
list =
    Block.list


list2 : (g -> String -> BlockI t i a) -> g -> String -> BlockI t (List i) (List a)
list2 =
    Block.list2


string : String -> Block t String
string =
    Block.string


int : String -> Block t Int
int =
    Block.int


float : String -> Block t Float
float =
    Block.float


stringEntryBlock :
    { toString : a -> String
    , toType : a -> Type t
    , fromString : String -> Maybe a
    , fromType : Type t -> Maybe a
    , default : a
    , onError : String -> String
    }
    -> String
    -> Block t a
stringEntryBlock =
    Block.stringEntryBlock


oneOf : ( a, String ) -> List ( a, String ) -> String -> Block t a
oneOf =
    Block.oneOf


bool : String -> Block t Bool
bool =
    oneOf ( True, "True" ) [ ( False, "False" ) ]


custom : (t -> Maybe a) -> (a -> t) -> a -> BlockI t a a
custom =
    Block.custom

{-| Override the default value for a block. Use this to get a stable default
to be referenced in multiple places. Use this when using
Component.Application.updateAt.

withDefault is used to set the initial value when building Components with
withControl, withState etc., but not when using withControl_, withState_, etc.
-}
withDefault : i -> Block.BlockI t i a -> Block.BlockI t i a
withDefault = Block.withDefault