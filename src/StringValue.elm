module StringValue exposing
    ( Block(..)
    , Config(..)
    , Metadata
    , Model
    , Msg(..)
    , Ops(..)
    , canCancel
    , canConfirm
    , canConfirmOrDenyOrDelete
    , canDelete
    , canSwitchToEdit
    , getFixed
    , isCreating
    , isEditing
    , isFixed
    , isGone
    , update
    , view
    )

import Debug exposing (log)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class, size, title, value)
import Html.Events exposing (onInput)
import Lib exposing (noOp, setCons)


type Ops
    = CanDelete
    | CanCancel


type alias Metadata =
    List Ops


type Block
    = Fixed ( Metadata, String ) -- with toolbar, but otherwise fixed
    | Editing ( Metadata, String, String ) -- (..., original, proposed)
    | Creating ( Metadata, String ) -- (..., proposed)
    | Gone -- either cancelled, or does not exist


type alias Model =
    Block


type Config msg
    = Config
        { size : Int
        , description : String
        , toMsg : Msg -> msg
        , toolMap : List ( Model -> Bool, Model -> List (Maybe (Html msg)) )
        , customHtml : List ( Model -> Bool, Model -> Html msg -> Html msg )
        }


type Msg
    = Edit
    | Create Metadata
    | Delete
    | Confirm
    | Cancel
    | ContentChange String


isFixed : Block -> Bool
isFixed x =
    case x of
        Fixed _ ->
            True

        _ ->
            False


isEditing : Block -> Bool
isEditing x =
    case x of
        Editing _ ->
            True

        _ ->
            False


isCreating : Block -> Bool
isCreating x =
    case x of
        Creating _ ->
            True

        _ ->
            False


isGone : Block -> Bool
isGone x =
    case x of
        Gone ->
            True

        _ ->
            False


getFixed : Block -> Maybe String
getFixed x =
    case x of
        Fixed ( _, v ) ->
            Just v

        _ ->
            Nothing



-- This is not a fixed value


metadata : Block -> Maybe Metadata
metadata x =
    case x of
        Fixed ( m, _ ) ->
            Just m

        Editing ( m, _, _ ) ->
            Just m

        Creating ( m, _ ) ->
            Just m

        _ ->
            Nothing



-- Can't get the metadata for this value


canDelete : Block -> Bool
canDelete x =
    metadata x
        |> Maybe.map (List.member CanDelete)
        |> Maybe.withDefault (log "issue in canDelete" False)


canCancel : Block -> Bool
canCancel x =
    case x of
        Fixed _ ->
            False

        _ ->
            metadata x
                |> Maybe.map (List.member CanCancel)
                |> Maybe.withDefault (log "issue in canCancel" False)


canConfirm : Block -> Bool
canConfirm x =
    let
        lenCheck v =
            String.length (String.trim v) > 0
    in
    case x of
        Editing ( _, _, p ) ->
            lenCheck p

        Creating ( _, p ) ->
            lenCheck p

        _ ->
            False


canConfirmOrDenyOrDelete : Block -> Bool
canConfirmOrDenyOrDelete x =
    canConfirm x || canCancel x || canDelete x


canSwitchToEdit : Block -> Bool
canSwitchToEdit x =
    case x of
        Fixed _ ->
            True

        _ ->
            False



-- helper for `update`


optionalOp : Metadata -> Ops -> Block
optionalOp allowed v =
    if allowed |> List.member v then
        Gone

    else
        log "OptionalOp not applicable" Gone


update : Msg -> Model -> Model
update msg model =
    case ( model, msg ) of
        ( Gone, Create meta ) ->
            Creating ( meta, "" )

        ( Fixed ( meta, o ), Edit ) ->
            Editing ( setCons CanCancel meta, o, o )

        ( Editing ( meta, _, p ), Confirm ) ->
            Fixed ( meta, p )

        ( Creating ( meta, p ), Confirm ) ->
            Fixed ( meta, p )

        ( Editing ( meta, _, _ ), Delete ) ->
            optionalOp meta CanDelete

        ( Creating ( meta, _ ), Delete ) ->
            optionalOp meta CanDelete

        ( Fixed ( meta, _ ), Delete ) ->
            optionalOp meta CanDelete

        ( Editing ( meta, o, _ ), Cancel ) ->
            Fixed ( meta, o )

        ( Editing ( meta, o, _ ), ContentChange s ) ->
            Editing ( meta, o, s )

        ( Creating ( meta, _ ), ContentChange s ) ->
            Creating ( meta, s )

        -- these next transitions may occur repeatedly due to the UI, that's OK
        ( _, Confirm ) ->
            model

        ( _, Cancel ) ->
            model

        _ ->
            log "The state/message combination were invalid, in StringValue update!" ( model, msg )
                |> (\_ -> model)


view : Config msg -> Model -> Html msg
view (Config config) model =
    let
        toolBlock =
            case config.toolMap |> List.filter (\( f, _ ) -> f model) of
                ( _, html ) :: _ ->
                    Lib.someHtml (div [ class "tools" ]) (html model)

                [] ->
                    Lib.noOp

        block orig proposed =
            input
                [ size config.size
                , onInput (ContentChange >> config.toMsg)
                , value proposed -- CHECK: This was altered from original, where is `defaultValue` in 0.19??
                , title ("Change " ++ config.description)
                ]
                [ toolBlock ]
    in
    case config.customHtml |> List.filter (\( f, _ ) -> f model) of
        ( _, custom ) :: _ ->
            custom model toolBlock

        [] ->
            case model of
                Gone ->
                    noOp

                Fixed ( meta, s ) ->
                    div [] [ text s, toolBlock ]

                Editing ( meta, o, v ) ->
                    div [] [ block o v, toolBlock ]

                Creating ( meta, v ) ->
                    div [] [ block "" v, toolBlock ]
