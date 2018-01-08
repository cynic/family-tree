module StringValue exposing
  ( Ops(..), Metadata, Block(..), Model, Msg(..), Config(Config)
  , isFixed, getFixed, isEditing, isCreating, isGone
  , canDelete, canCancel, canConfirm, canConfirmOrDenyOrDelete, canSwitchToEdit
  , update, view
  )
import Debug exposing (crash)
import Html exposing (input, text, Html, div, span)
import Html.Attributes exposing (defaultValue, value, size, title, class)
import Html.Events exposing (onInput)
import Lib exposing (noOp, setCons)

type Ops =
  CanDelete
  | CanCancel

type alias Metadata = List Ops

type Block =
  Fixed (Metadata, String) -- with toolbar, but otherwise fixed
  | Editing (Metadata, String, String) -- (..., original, proposed)
  | Creating (Metadata, String) -- (..., proposed)
  | Gone -- either cancelled, or does not exist

type alias Model = Block

type Config msg =
  Config
    { size : Int
    , overrideValue : Bool
    , description : String
    , toMsg : Msg -> msg
    , toolMap : List (Model -> Bool, Model -> List (Maybe (Html msg)))
    , customHtml : List (Model -> Bool, Model -> Html msg -> Html msg)
    }

type Msg =
  Edit
  | Create Metadata
  | Delete
  | Confirm
  | Cancel
  | ContentChange String

isFixed : Block -> Bool
isFixed x =
  case x of
    Fixed _ -> True
    _ -> False

isEditing : Block -> Bool
isEditing x =
  case x of
    Editing _ -> True
    _ -> False

isCreating : Block -> Bool
isCreating x =
  case x of
    Creating _ -> True
    _ -> False

isGone : Block -> Bool
isGone x =
  case x of
    Gone -> True
    _ -> False

getFixed : Block -> String
getFixed x =
  case x of
    Fixed (_,v) -> v
    _ -> crash "This is not a fixed value"

metadata : Block -> Metadata
metadata x =
  case x of
    Fixed (m,_) -> m
    Editing (m,_,_) -> m
    Creating (m,_) -> m
    _ -> crash "Can't get the metadata for this value"

canDelete : Block -> Bool
canDelete x =
  metadata x |> List.member CanDelete

canCancel : Block -> Bool
canCancel x =
  case x of
    Fixed _ -> False
    _ -> metadata x |> List.member CanCancel

canConfirm : Block -> Bool
canConfirm x =
  let
    lenCheck x = String.length (String.trim x) > 0
  in
    case x of
      Editing (_, _, p) -> lenCheck p
      Creating (_, p) -> lenCheck p
      _ -> False

canConfirmOrDenyOrDelete : Block -> Bool
canConfirmOrDenyOrDelete x =
  canConfirm x || canCancel x || canDelete x

canSwitchToEdit : Block -> Bool
canSwitchToEdit x =
  case x of
    Fixed _ -> True
    _ -> False

update : Msg -> Model -> Model
update msg model =
  let
    optionalOp allowed v =
      if allowed |> List.member v then Gone
      else crash "OptionalOp not applicable"
  in
    case (model, msg) of
      (Gone, Create meta) ->
        Creating (meta,"")
      (Fixed (meta, o), Edit) ->
        Editing (setCons CanCancel meta,o,o)
      (Editing (meta,_,p), Confirm) ->
        Fixed (meta,p)
      (Creating (meta,p), Confirm) ->
        Fixed (meta,p)
      (Editing (meta,_,_), Delete) ->
        optionalOp meta CanDelete
      (Creating (meta,_), Delete) ->
        optionalOp meta CanDelete
      (Fixed (meta,_), Delete) ->
        optionalOp meta CanDelete
      (Editing (meta,o,_), Cancel) ->
        Fixed (meta,o)
      (Editing (meta,o,_), ContentChange s) ->
        Editing (meta,o,s)
      (Creating (meta,_), ContentChange s) ->
        Creating (meta,s)
      -- these next transitions may occur repeatedly due to the UI, that's OK
      (_, Confirm) -> model
      (_, Cancel) -> model
      _ -> crash "Invalid state"

view : Config msg -> Model -> Html msg
view (Config config) model =
  let
    toolBlock =
      case config.toolMap |> List.filter (\(f,_) -> f model) of
        (_,html)::_ -> Lib.someHtml (div [class "tools"]) (html model)
        [] -> Lib.noOp
    block orig proposed =
      input
        [ size config.size
        , onInput (ContentChange >> config.toMsg)
        , (if config.overrideValue then value else defaultValue) proposed
        , title ("Change " ++ config.description)
        ] [toolBlock]
  in
    case config.customHtml |> List.filter (\(f,_) -> f model) of
      (_,custom)::_ ->
        custom model toolBlock
      [] ->
        case model of
          Gone -> noOp
          Fixed (meta, s) -> div [] [text s, toolBlock]
          Editing (meta, o, v) -> div [] [block o v, toolBlock]
          Creating (meta, v) -> div [] [block "" v, toolBlock]
