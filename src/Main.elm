import Html exposing (div, span, Html, input, text, beginnerProgram, node)
import Html.Attributes exposing (class, classList, title, attribute, size, defaultValue)
import Html.Events exposing (onInput, onClick)
import List exposing (indexedMap, map, concat, any, all, filter)
import Debug exposing (log, crash)
import NameBlock as Block exposing (NameKind(..), NameOps(..))
import Lib exposing (..)

-- MODEL

type alias FullName =
  { first : String
  , middle : List String
  , last : String
  }

type alias EditableName =
  { first : Block.Model
  , middle : List Block.Model
  , last : Block.Model
  }

type WhichName =
  First
  | Last
  | Middle Int

type GlobalNameState =
  AtRest
  | BeingCreated
  | EditRequested FullName

type Name =
  Editable EditableName
  | Resolved FullName

type alias Model =
  { global : GlobalNameState
  , data : Name
  -- When the middle names list is manipulated, we want to reset the values
  -- in the respective boxes, if they're in an Editing state.  Ordinarily, we don't
  -- want to do this, because it makes typing into the boxes a real pain.
  , overrideMiddleValues : Bool
  }

-- UPDATE

type Msg =
  EditFullNameClick
  | MoveRight Int
  | MoveLeft Int
  | AppendMiddle
  | GlobalConfirm
  | GlobalUndo
  | SubMsg (WhichName, Block.Msg)

-- update-specific types

type GlobalNameState_Coalesce = AtRest_ | EditingOrCreating GlobalNameState
global_coalesce : GlobalNameState -> GlobalNameState_Coalesce
global_coalesce global =
  case global of
    AtRest -> AtRest_
    _ -> EditingOrCreating global

isEditing : GlobalNameState -> Bool
isEditing global =
  case global of
    EditRequested _ -> True
    _ -> False

canMoveRight : WhichName -> List Block.Model -> Bool
canMoveRight which middle =
  case which of
    Middle n ->
      n+1 < List.length middle
    _ -> False

canMoveLeft : WhichName -> Bool
canMoveLeft which =
  case which of
    Middle 0 -> False
    Middle _ -> True
    _ -> False

middleIndex : WhichName -> Int
middleIndex which =
  case which of
    Middle n -> n
    _ -> crash "Can't find middle index of non-middle name"

update : Msg -> Model -> Model
update msg model_ =
  let
    model = {model_ | overrideMiddleValues=False} -- reset!
    {global,data} = model
    editToFull {first, middle, last} =
      if all Block.isFixed (first::last::middle) then
        {first=Block.getFixed first, last=Block.getFixed last, middle=map Block.getFixed middle}
      else
        crash "All blocks aren't fixed"
    fullToNew {first, middle, last} =
      let
        f = Block.Fixed ({kind=Ordinary, allowed=[CanCancel]}, first)
        m v = Block.Fixed ({kind=Ordinary, allowed=[CanDelete,CanCancel]}, v)
        l = Block.Fixed ({kind=Surname, allowed=[CanCancel]}, last)
      in {first=f, middle=map m middle, last=l}
    getBlock which {first, middle, last} =
      case which of
        First -> first
        Last -> last
        Middle n -> nth n middle
    updateSub msg which name =
      case (which, Block.update msg (getBlock which name)) of
        (First, Block.Gone) -> crash "First name is mandatory"
        (Last, Block.Gone) -> crash "Last name is mandatory"
        (Middle n, Block.Gone) -> {name | middle=removeAt n name.middle}
        (First, v) -> {name | first=v}
        (Last, v) -> {name | last=v}
        (Middle n, v) -> {name | middle=replaceAt n v name.middle}
  in
    case (global_coalesce global,msg,data) of
      (AtRest_, EditFullNameClick, Resolved name) ->
        {model | global=EditRequested name, data=Editable (fullToNew name)}
      (EditingOrCreating _, SubMsg (which, msg), Editable name) ->
        case msg of
          Block.Delete -> {model | data=Editable (updateSub msg which name), overrideMiddleValues=True}
          _ -> {model | data=Editable (updateSub msg which name), overrideMiddleValues=True}
      (EditingOrCreating _, GlobalConfirm, Editable name) ->
        {model | global=AtRest, data=Resolved (editToFull name)}
      (EditingOrCreating (EditRequested name), GlobalUndo, _) ->
        {model | global=AtRest, data=Resolved name}
      (EditingOrCreating _, MoveLeft n, Editable name) ->
        {model | data=Editable {name | middle=swap n (n-1) name.middle}, overrideMiddleValues=True}
      (EditingOrCreating _, MoveRight n, Editable name) ->
        {model | data=Editable {name | middle=swap n (n+1) name.middle}, overrideMiddleValues=True}
      (EditingOrCreating _, AppendMiddle, Editable name) ->
        let
          newMiddle = Block.Creating ({kind=Ordinary, allowed=[CanDelete]}, "")
        in
          {model | data=Editable {name | middle=name.middle ++ [newMiddle]}, overrideMiddleValues=True}
      _ -> crash "This transition is not valid"

view : Model -> Html Msg
view {global, data, overrideMiddleValues} =
  case (global_coalesce global, data) of
    (AtRest_, Resolved {first,middle,last}) ->
      let
        firstDiv = div [class "first"] [text first]
        middleDivs = map (\v -> div [class "middle"] [text v]) middle
        lastDiv = div [class "last"] [text last]
      in
        div [class "name full", title "Click to edit", onClick EditFullNameClick]
          (firstDiv :: middleDivs ++ [lastDiv])
    (EditingOrCreating _, Editable name) ->
      let
        {first, middle, last} = name
        fa_button icon title_ click =
          span [class ("fa fa-" ++ icon), title title_, onClick click] []
        big_fa_button icon title_ click =
          fa_button ("2x fa-" ++ icon) title_ click
        globalConfirm () =
          big_fa_button "check" "Confirm changes" GlobalConfirm
        globalUndo () =
          big_fa_button "times" "Undo changes" GlobalUndo
        toolsOf which name =
          let
            leftButton () =
              fa_button "arrow-circle-o-left" "Move left" (MoveLeft (middleIndex which))
            rightButton () =
              fa_button "arrow-circle-o-right" "Move right" (MoveRight (middleIndex which))
            editButton () =
              fa_button "edit" ("Change " ++ Block.kindString name) (SubMsg (which, Block.Edit))
            deleteButton () =
              fa_button "trash" ("Delete " ++ Block.kindString name) (SubMsg (which, Block.Delete))
            confirmButton () =
              fa_button "check" "Confirm" (SubMsg (which, Block.Confirm))
            cancelButton () =
              fa_button "times" "Cancel" (SubMsg (which, Block.Cancel))
          in
            someHtml (div [class "tools"])
              [ optionally (canMoveLeft which) leftButton
              , optionally (Block.canSwitchToEdit name) editButton
              , optionally (canMoveRight which middle) rightButton
              , optionally (Block.canConfirmOrDenyOrDelete name)
                (\() -> someHtml (div [class "sub-confirm-edit"])
                  [ optionally (Block.canConfirm name) confirmButton
                  , optionally (Block.canCancel name) cancelButton
                  , optionally (Block.canDelete name) deleteButton
                  ]
                )
              ]
        firstHtml =
          div [class "edit-item first"]
            [ Block.view first False |> Html.map (\v -> SubMsg (First, v))
            , toolsOf First first
            ]
        middleHtml =
          middle |> indexedMap
            ( \idx -> \v -> Block.view v overrideMiddleValues
              |> Html.map (\x -> SubMsg (Middle idx, x))
              |> (\content ->
                div [class "edit-item middle"]
                [ content
                , toolsOf (Middle idx) v
                ]
              )
            )
        appendButton =
          div [class "edit-item"] [big_fa_button "plus-circle" "Add new middle name" AppendMiddle]
        lastHtml =
          div [class "edit-item last"]
            [ Block.view last False |> Html.map (\v -> SubMsg (Last, v))
            , toolsOf Last last
            ]
      in
        div [class "name editing"]
          [ div [class "edit-items"]
            (List.concat [ [firstHtml], middleHtml, [appendButton], [lastHtml] ])
          , elideUnless (all Block.isFixed (first::last::middle)) (div [class "confirm-edit"])
            [ Just (globalConfirm ())
            , optionally (isEditing global) globalUndo
            ]
          ]
    _ -> crash "Invalid state"

main : Program Basics.Never Model Msg
main =
  let
    initial =
      { global=BeingCreated
      , data=Editable
        { first=Block.Creating ({kind=Block.Ordinary, allowed=[]}, "")
        , middle=[]
        , last=Block.Creating ({kind=Block.Surname, allowed=[]}, "")
        }
      , overrideMiddleValues=False
      }
  in
    beginnerProgram
      { model = initial
      , update = update
      , view = view
      }
