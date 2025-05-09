module Component exposing
    ( Block
    , Builder
    , Library
    , Lookup
    , Msg
    , Preview
    , PreviewRef
    , addVia
    , bool
    , build
    , finish
    , finish_
    , float
    , fromPreview
    , identifier
    , int
    , list
    , list2
    , map
    , oneOf
    , preview
    , previewBlock
    , string
    , stringEntryBlock
    , withControl
    , withControl_
    , withMsg
    , withPreview
    , withPreview_
    , withState
    , withState_
    , withUnlabelled
    , withUnlabelled_
    )

import Component.Block as Block
import Component.Preview as Preview
import Component.Ref as Ref
import Component.Type exposing (Type)
import Html exposing (Html)


type alias Library t msg =
    Preview.Library t msg


type alias Preview t msg a =
    Preview.Preview t msg a


type alias Block t a =
    Block.Block t a


type alias BlockI t i a =
    Block.BlockI t i a


type alias Lookup t =
    Block.Lookup t


type alias Msg t msg =
    Preview.Msg t msg


type alias Builder t i r a =
    Block.Builder t i r a


type alias PreviewRef =
    Preview.PreviewRef


type alias Ref =
    Ref.Ref


map : (a -> b) -> Preview t msg a -> Preview t msg b
map =
    Preview.map


preview : String -> { name : String } -> a -> Preview t msg a
preview =
    Preview.preview


previewBlock : Library t msg -> String -> BlockI t PreviewRef (Html msg)
previewBlock =
    Preview.previewBlock


fromPreview : Preview t msg a -> PreviewRef
fromPreview =
    Preview.fromPreview


withControl : String -> (String -> Block t a) -> a -> Preview t msg (a -> b) -> Preview t msg b
withControl label block default =
    Preview.withControl label (\l -> Block.withDefault default (block l))


withControl_ : String -> (String -> Block t a) -> Preview t msg (a -> b) -> Preview t msg b
withControl_ =
    Preview.withControl


withMsg : Preview t (Msg t m) ((m -> Msg t m) -> a) -> Preview t (Msg t m) a
withMsg =
    Preview.withMsg


withState : String -> (String -> BlockI t i a) -> i -> Preview t (Msg t msg) (a -> (i -> Msg t msg) -> c) -> Preview t (Msg t msg) c
withState label block default =
    Preview.withState label (\l -> Block.withDefault default (block l))


withState_ : String -> (String -> BlockI t i a) -> Preview t (Msg t msg) (a -> (i -> Msg t msg) -> c) -> Preview t (Msg t msg) c
withState_ =
    Preview.withState


withPreview : String -> (Preview.Library t msg -> String -> Block.BlockI t i b) -> i -> Preview.Preview t msg (b -> a) -> Preview.Preview t msg a
withPreview label block default =
    Preview.withPreview label (\lib l -> Block.withDefault default (block lib l))


withPreview_ : String -> (Preview.Library t msg -> String -> Block.BlockI t i b) -> Preview.Preview t msg (b -> a) -> Preview.Preview t msg a
withPreview_ =
    Preview.withPreview


withUnlabelled : BlockI t i a -> i -> Preview t msg (a -> b) -> Preview t msg b
withUnlabelled block default =
    Preview.withUnlabelled (Block.withDefault default block)


withUnlabelled_ : BlockI t i a -> Preview t msg (a -> b) -> Preview t msg b
withUnlabelled_ =
    Preview.withUnlabelled


addVia : (r -> a) -> String -> (String -> BlockI t a a) -> Builder t (a -> b) r (a -> b) -> Builder t b r b
addVia =
    Block.addVia


build : a -> Builder t a r a
build =
    Block.build


finish : (i -> a) -> Block.Builder t i i i -> String -> Block.BlockI t i a
finish f =
    Block.finishI f


finish_ : Block.Builder t a a a -> String -> Block.BlockI t a a
finish_ =
    Block.finishI identity


identifier : BlockI t Ref String
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
