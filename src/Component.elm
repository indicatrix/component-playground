module Component exposing
    ( Block
    , BlockI
    , Builder
    , Library
    , Lookup
    , Msg
    , Preview
    , PreviewRef
    , Ref
    , View
    , addVia
    , bool
    , build
    , custom
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
    , toComponentMsg
    , withControl
    , withControl_
    , withMsg
    , withMsg2
    , withMsg3
    , withPreview
    , withPreview_
    , withState
    , withStateF
    , withStateF_
    , withState_
    , withUnlabelled
    , withUnlabelledState
    , withUnlabelledStateF
    , withUnlabelledStateF_
    , withUnlabelledState_
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


type alias View msg =
    Preview.View msg


type alias Builder t i r a =
    Block.Builder t i r a


type alias PreviewRef =
    Preview.PreviewRef


type alias Ref =
    Ref.Ref


toComponentMsg : msg -> Msg t msg
toComponentMsg msg =
    Preview.Msg [] msg


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


withMsg : (a -> msg) -> Preview t (Msg t msg) ((a -> Msg t msg) -> r) -> Preview t (Msg t msg) r
withMsg =
    Preview.withMsg


withMsg2 : (a -> b -> msg) -> Preview t (Msg t msg) ((a -> b -> Msg t msg) -> r) -> Preview t (Msg t msg) r
withMsg2 =
    Preview.withMsg2


withMsg3 : (a -> b -> c -> msg) -> Preview t (Msg t msg) ((a -> b -> c -> Msg t msg) -> r) -> Preview t (Msg t msg) r
withMsg3 =
    Preview.withMsg3


withState : String -> (String -> BlockI t i a) -> i -> Preview t (Msg t msg) (a -> (i -> Msg t msg) -> y) -> Preview t (Msg t msg) y
withState label blockF default =
    withState_ label (\l -> Block.withDefault default (blockF l))


withState_ : String -> (String -> BlockI t i a) -> Preview t (Msg t msg) (a -> (i -> Msg t msg) -> y) -> Preview t (Msg t msg) y
withState_ label blockF =
    withUnlabelledState_ (blockF label)


withStateF : String -> (String -> BlockI t i a) -> i -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y) -> Preview t (Msg t msg) x -> Preview t (Msg t msg) y
withStateF label blockF default =
    withStateF_ label (\l -> Block.withDefault default (blockF l))


withStateF_ : String -> (String -> BlockI t i a) -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y) -> Preview t (Msg t msg) x -> Preview t (Msg t msg) y
withStateF_ label blockF =
    withUnlabelledStateF_ (blockF label)


withUnlabelledState : BlockI t i a -> i -> Preview t (Msg t msg) (a -> (i -> Msg t msg) -> b) -> Preview t (Msg t msg) b
withUnlabelledState block default =
    withUnlabelledState_ (Block.withDefault default block)


withUnlabelledState_ : BlockI t i a -> Preview t (Msg t msg) (a -> (i -> Msg t msg) -> b) -> Preview t (Msg t msg) b
withUnlabelledState_ block =
    Preview.withState block (\get set f -> f get set)


withUnlabelledStateF : BlockI t i a -> i -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y) -> Preview t (Msg t msg) x -> Preview t (Msg t msg) y
withUnlabelledStateF block default =
    withUnlabelledStateF_ (Block.withDefault default block)


withUnlabelledStateF_ : BlockI t i a -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y) -> Preview t (Msg t msg) x -> Preview t (Msg t msg) y
withUnlabelledStateF_ =
    Preview.withStateF


withPreview : String -> (Library t msg -> String -> BlockI t i b) -> i -> Preview t msg (b -> a) -> Preview t msg a
withPreview label block default =
    Preview.withPreview label (\lib l -> Block.withDefault default (block lib l))


withPreview_ : String -> (Library t msg -> String -> BlockI t i b) -> Preview t msg (b -> a) -> Preview t msg a
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


finish : (i -> a) -> Builder t i i i -> String -> BlockI t i a
finish f =
    Block.finishI f


finish_ : Builder t a a a -> String -> BlockI t a a
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


custom : (t -> Maybe a) -> (a -> t) -> a -> BlockI t a a
custom =
    Block.custom
