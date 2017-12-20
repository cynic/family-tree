module NameBlock exposing
  ( NameKind(..), NameOps(..), Metadata, NameBlock(..), Model, Msg(..)
  , isFixed, getFixed, kindString
  , canDelete, canCancel, canConfirm, canConfirmOrDenyOrDelete, canSwitchToEdit
  , update, view
  )
import Debug exposing (crash)
import Html exposing (input, text, Html)
import Html.Attributes exposing (defaultValue, value, size, title)
import Html.Events exposing (onInput)
import Lib exposing (noOp, setCons)

type NameKind =
  Ordinary
  | Surname

type NameOps =
  CanDelete
  | CanCancel

type alias Metadata =
  { kind : NameKind
  , allowed : List NameOps
  }

type NameBlock =
  Fixed (Metadata, String) -- with toolbar, but otherwise fixed
  | Editing (Metadata, String, String) -- (..., original, proposed)
  | Creating (Metadata, String) -- (..., proposed)
  | Gone -- either cancelled, or does not exist

type alias Model = NameBlock

type Msg =
  Edit
  | Delete
  | Confirm
  | Cancel
  | NameContentChange String

isFixed : NameBlock -> Bool
isFixed x =
  case x of
    Fixed _ -> True
    _ -> False

getFixed : NameBlock -> String
getFixed x =
  case x of
    Fixed (_,v) -> v
    _ -> crash "This is not a fixed value"

metadata : NameBlock -> Metadata
metadata x =
  case x of
    Fixed (m,_) -> m
    Editing (m,_,_) -> m
    Creating (m,_) -> m
    _ -> crash "Can't get the metadata for this value"

kindString : NameBlock -> String
kindString x =
  case .kind (metadata x) of
    Ordinary -> "name"
    Surname -> "surname"

canDelete : NameBlock -> Bool
canDelete x =
  .allowed (metadata x) |> List.member CanDelete

canCancel : NameBlock -> Bool
canCancel x =
  case x of
    Fixed _ -> False
    _ -> .allowed (metadata x) |> List.member CanCancel

canConfirm : NameBlock -> Bool
canConfirm x =
  let
    lenCheck x = String.length (String.trim x) > 0
  in
    case x of
      Editing (_, _, p) -> lenCheck p
      Creating (_, p) -> lenCheck p
      _ -> False

canConfirmOrDenyOrDelete : NameBlock -> Bool
canConfirmOrDenyOrDelete x =
  canConfirm x || canCancel x || canDelete x

canSwitchToEdit : NameBlock -> Bool
canSwitchToEdit x =
  case x of
    Fixed _ -> True
    _ -> False

update : Msg -> Model -> Model
update msg model =
  let
    optionalOp {allowed} v =
      if allowed |> List.member v then Gone
      else crash "OptionalOp not applicable"
  in
    case msg of
      Edit ->
        case model of
          Fixed (meta,o) -> Editing ({meta | allowed=setCons CanCancel meta.allowed},o,o)
          _ -> crash "Invalid state for SubEditName"
      Confirm ->
        case model of
          Editing (meta,_,p) -> Fixed (meta,p)
          Creating (meta,p) -> Fixed (meta,p)
          _ -> crash "Invalid state for SubConfirm"
      Delete ->
        case model of
          Editing (meta,_,_) -> optionalOp meta CanDelete
          Creating (meta,_) -> optionalOp meta CanDelete
          Fixed (meta, _) -> optionalOp meta CanDelete
          _ -> crash "Invalid state for SubDelete"
      Cancel ->
        case model of
          Editing (meta,o,_) -> Fixed (meta,o)
          _ -> crash "Invalid state for SubUndo"
      NameContentChange s ->
        case model of
          Editing (meta,o,_) -> Editing (meta,o,s)
          Creating (meta,_) -> Creating (meta,s)
          _ -> crash "Invalid state for NameContentChange"

view : Model -> Bool -> Html Msg
view model overrideValue =
  let
    block orig proposed =
      input
        [ size 12
        , onInput NameContentChange
        , (if overrideValue then value else defaultValue) proposed
        , title ("Change " ++ kindString model)
        ] []
  in
    case model of
      Gone -> noOp
      Fixed (meta, s) -> text s
      Editing (meta, o, v) -> block o v
      Creating (meta, v) -> block "" v
