module NameBlock exposing
    ( Metadata
    , Model
    , Msg(..)
    , NameBlock(..)
    , NameKind(..)
    , NameOps(..)
    , canCancel
    , canConfirm
    , canConfirmOrDenyOrDelete
    , canDelete
    , canSwitchToEdit
    , getFixed
    , isFixed
    , kindString
    , update
    , view
    )

import Debug exposing (log)
import Html exposing (Html, input, text)
import Html.Attributes exposing (size, title, value)
import Html.Events exposing (onInput)
import Lib exposing (noOp, setCons)


type NameKind
    = First
    | Middle
    | Nickname
    | Surname


type NameOps
    = CanDelete
    | CanCancel


type alias Metadata =
    { kind : NameKind
    , allowed : List NameOps
    }


type NameBlock
    = Fixed ( Metadata, String ) -- with toolbar, but otherwise fixed
    | Editing ( Metadata, String, String ) -- (..., original, proposed)
    | Creating ( Metadata, String ) -- (..., proposed)
    | Gone -- either cancelled, or does not exist


type alias Model =
    NameBlock


type Msg
    = Edit
    | Delete
    | Confirm
    | Cancel
    | NameContentChange String


isFixed : NameBlock -> Bool
isFixed x =
    case x of
        Fixed _ ->
            True

        _ ->
            False


getFixed : NameBlock -> Maybe String
getFixed x =
    case x of
        Fixed ( _, v ) ->
            Just v

        _ ->
            Nothing



-- This is not a fixed value


metadata : NameBlock -> Maybe Metadata
metadata x =
    case x of
        Fixed ( m, _ ) ->
            Just m

        Editing ( m, _, _ ) ->
            Just m

        Creating ( m, _ ) ->
            Just m

        Gone ->
            Nothing



-- Can't get the metadata for this value


kindString : NameBlock -> String
kindString x =
    case metadata x of
        Nothing ->
            "?????"

        Just v ->
            case .kind v of
                First ->
                    "first name"

                Middle ->
                    "middle name"

                Nickname ->
                    "nickname"

                Surname ->
                    "surname"


canDelete : NameBlock -> Bool
canDelete x =
    metadata x
        |> Maybe.map .allowed
        |> Maybe.map (List.member CanDelete)
        |> Maybe.withDefault (log "Asked to delete something, but metadata not found..." False)


canCancel : NameBlock -> Bool
canCancel x =
    case x of
        Fixed _ ->
            False

        _ ->
            metadata x
                |> Maybe.map .allowed
                |> Maybe.map (List.member CanCancel)
                |> Maybe.withDefault (log "Asked to cancel something, but metadata not found..." False)


canConfirm : NameBlock -> Bool
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


canConfirmOrDenyOrDelete : NameBlock -> Bool
canConfirmOrDenyOrDelete x =
    canConfirm x || canCancel x || canDelete x


canSwitchToEdit : NameBlock -> Bool
canSwitchToEdit x =
    case x of
        Fixed _ ->
            True

        _ ->
            False



-- helper for `update`


optionalOp : Metadata -> NameOps -> NameBlock
optionalOp { allowed } v =
    if allowed |> List.member v then
        Gone

    else
        log "OptionOp not applicable!! (this is an error...)" Gone


update : Msg -> Model -> Model
update msg model =
    case msg of
        Edit ->
            case model of
                Fixed ( meta, o ) ->
                    Editing ( { meta | allowed = setCons CanCancel meta.allowed }, o, o )

                _ ->
                    log "Invalid state (should be Fixed) for SubEditName" model

        Confirm ->
            case model of
                Editing ( meta, _, p ) ->
                    Fixed ( meta, p )

                Creating ( meta, p ) ->
                    Fixed ( meta, p )

                _ ->
                    log "Invalid state (should be Editing/Creating) for SubConfirm" model

        Delete ->
            case model of
                Editing ( meta, _, _ ) ->
                    optionalOp meta CanDelete

                Creating ( meta, _ ) ->
                    optionalOp meta CanDelete

                Fixed ( meta, _ ) ->
                    optionalOp meta CanDelete

                _ ->
                    log "Invalid state (should be Editing/Creating/Fixed) for SubDelete" model

        Cancel ->
            case model of
                Editing ( meta, o, _ ) ->
                    Fixed ( meta, o )

                _ ->
                    log "Invalid state (should be Editing) for SubUndo" model

        NameContentChange s ->
            case model of
                Editing ( meta, o, _ ) ->
                    Editing ( meta, o, s )

                Creating ( meta, _ ) ->
                    Creating ( meta, s )

                _ ->
                    log "Invalid state (should be Editing/Creating) for NameContentChange" model


view : Model -> Html Msg
view model =
    let
        block orig proposed =
            input
                [ size 12
                , onInput NameContentChange
                , value proposed -- CHECK: Where's `defaultValue` in 0.19??!
                , title ("Change " ++ kindString model)
                ]
                []
    in
    case model of
        Gone ->
            noOp

        Fixed ( meta, s ) ->
            text s

        Editing ( meta, o, v ) ->
            block o v

        Creating ( meta, v ) ->
            block "" v
