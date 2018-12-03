module PrimaryName exposing
    ( Model
    , Msg
    , newName
    , update
    , view
    )

import Button exposing (Button(..))
import Debug exposing (log)
import Html exposing (Html, div, input, node, span, text)
import Html.Attributes exposing (attribute, class, classList, size, title)
import Html.Events exposing (onClick, onInput)
import Lib exposing (..)
import List exposing (all, any, concat, filter, indexedMap, map)
import NameBlock as Block exposing (NameKind(..), NameOps(..))



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


type WhichName
    = First
    | Last
    | Middle Int


type GlobalNameState
    = AtRest
    | BeingCreated
    | EditRequested FullName


type Name
    = Editable EditableName
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


type Msg
    = EditFullNameClick
    | MoveRight Int
    | MoveLeft Int
    | AppendMiddle
    | GlobalConfirm
    | GlobalUndo
    | SubMsg ( WhichName, Block.Msg )



-- update-specific types


type GlobalNameState_Coalesce
    = AtRest_
    | EditingOrCreating GlobalNameState


global_coalesce : GlobalNameState -> GlobalNameState_Coalesce
global_coalesce global =
    case global of
        AtRest ->
            AtRest_

        _ ->
            EditingOrCreating global


isEditing : GlobalNameState -> Bool
isEditing global =
    case global of
        EditRequested _ ->
            True

        _ ->
            False


canMoveRight : WhichName -> List Block.Model -> Bool
canMoveRight which middle =
    case which of
        Middle n ->
            n + 1 < List.length middle

        _ ->
            False


canMoveLeft : WhichName -> Bool
canMoveLeft which =
    case which of
        Middle 0 ->
            False

        Middle _ ->
            True

        _ ->
            False


middleIndex : WhichName -> Maybe Int
middleIndex which =
    case which of
        Middle n ->
            Just n

        _ ->
            Nothing



-- Can't find middle index of non-middle name


update : Msg -> Model -> Model
update received model_ =
    let
        model =
            { model_ | overrideMiddleValues = False }

        -- reset!
        { global, data } =
            model

        editToFull { first, middle, last } =
            if all Block.isFixed (first :: last :: middle) then
                { first = Block.getFixed first |> Maybe.withDefault "????", last = Block.getFixed last |> Maybe.withDefault "????", middle = map (Block.getFixed >> Maybe.withDefault "??") middle }

            else
                log "Error - All blocks aren't fixed" { first = "???", last = "???", middle = [] }

        fullToNew { first, middle, last } =
            let
                f =
                    Block.Fixed ( { kind = Block.First, allowed = [ CanCancel ] }, first )

                m v =
                    Block.Fixed ( { kind = Block.Middle, allowed = [ CanDelete, CanCancel ] }, v )

                l =
                    Block.Fixed ( { kind = Block.Surname, allowed = [ CanCancel ] }, last )
            in
            { first = f, middle = map m middle, last = l }

        getBlock which { first, middle, last } =
            case which of
                First ->
                    first

                Last ->
                    last

                Middle n ->
                    nth n middle
                        |> Maybe.withDefault Block.Gone

        updateSub msg which name =
            case ( which, Block.update msg (getBlock which name) ) of
                ( First, Block.Gone ) ->
                    log "Error - First name is mandatory" name

                ( Last, Block.Gone ) ->
                    log "Error - Last name is mandatory" name

                ( Middle n, Block.Gone ) ->
                    { name | middle = removeAt n name.middle }

                ( First, v ) ->
                    { name | first = v }

                ( Last, v ) ->
                    { name | last = v }

                ( Middle n, v ) ->
                    { name | middle = replaceAt n v name.middle }
    in
    case ( global_coalesce global, received, data ) of
        ( AtRest_, EditFullNameClick, Resolved name ) ->
            { model | global = EditRequested name, data = Editable (fullToNew name) }

        ( EditingOrCreating _, SubMsg ( which, msg ), Editable name ) ->
            case msg of
                Block.Delete ->
                    { model | data = Editable (updateSub msg which name), overrideMiddleValues = True }

                _ ->
                    { model | data = Editable (updateSub msg which name), overrideMiddleValues = True }

        ( EditingOrCreating _, GlobalConfirm, Editable name ) ->
            { model | global = AtRest, data = Resolved (editToFull name) }

        ( EditingOrCreating (EditRequested name), GlobalUndo, _ ) ->
            { model | global = AtRest, data = Resolved name }

        ( EditingOrCreating _, MoveLeft n, Editable name ) ->
            { model | data = Editable { name | middle = swap n (n - 1) name.middle |> Maybe.withDefault name.middle }, overrideMiddleValues = True }

        ( EditingOrCreating _, MoveRight n, Editable name ) ->
            { model | data = Editable { name | middle = swap n (n + 1) name.middle |> Maybe.withDefault name.middle }, overrideMiddleValues = True }

        ( EditingOrCreating _, AppendMiddle, Editable name ) ->
            let
                newMiddle =
                    Block.Creating ( { kind = Block.Middle, allowed = [ CanDelete ] }, "" )
            in
            { model | data = Editable { name | middle = name.middle ++ [ newMiddle ] }, overrideMiddleValues = True }

        _ ->
            log "This model/msg is not valid in PrimaryName" ( model, received )
                |> (\_ -> model)



-- helper for View


toolsOf : WhichName -> Block.NameBlock -> EditableName -> Html Msg
toolsOf which name { first, middle, last } =
    let
        left () =
            middleIndex which
                |> Maybe.map (\idx -> widget Button.MoveLeft (MoveLeft idx))
                |> Maybe.withDefault noOp

        right () =
            middleIndex which
                |> Maybe.map (\idx -> widget Button.MoveRight (MoveRight idx))
                |> Maybe.withDefault noOp

        edit () =
            widget (Button.Edit <| Block.kindString name) (SubMsg ( which, Block.Edit ))

        delete () =
            widget (Button.Delete <| Block.kindString name) (SubMsg ( which, Block.Delete ))

        confirm () =
            widget Button.Confirm (SubMsg ( which, Block.Confirm ))

        cancel () =
            widget Button.Cancel (SubMsg ( which, Block.Cancel ))
    in
    someHtml (div [ class "tools" ])
        [ optionally (canMoveLeft which) left
        , optionally (Block.canSwitchToEdit name) edit
        , optionally (canMoveRight which middle) right
        , optionally (Block.canConfirmOrDenyOrDelete name)
            (\() ->
                someHtml (div [ class "sub-confirm-edit" ])
                    [ optionally (Block.canConfirm name) confirm
                    , optionally (Block.canCancel name) cancel
                    , optionally (Block.canDelete name) delete
                    ]
            )
        ]


view : Model -> Html Msg
view { global, data, overrideMiddleValues } =
    case ( global_coalesce global, data ) of
        ( AtRest_, Resolved { first, middle, last } ) ->
            let
                firstDiv =
                    div [ class "first" ] [ text first ]

                middleDivs =
                    map (\v -> div [ class "middle" ] [ text v ]) middle

                lastDiv =
                    div [ class "last" ] [ text last ]
            in
            div [ class "primary" ]
                [ div [ class "full", title "Click to edit", onClick EditFullNameClick ]
                    (firstDiv :: middleDivs ++ [ lastDiv ])
                ]

        ( EditingOrCreating _, Editable name ) ->
            let
                { first, middle, last } =
                    name

                globalConfirm () =
                    widget (Big Button.Confirm) GlobalConfirm

                globalUndo () =
                    widget (Big Button.Cancel) GlobalUndo

                firstHtml =
                    div [ class "edit-item first" ]
                        [ Block.view first |> Html.map (\v -> SubMsg ( First, v ))
                        , toolsOf First first name
                        ]

                middleHtml =
                    middle
                        |> indexedMap
                            (\idx ->
                                \v ->
                                    Block.view v
                                        |> Html.map (\x -> SubMsg ( Middle idx, x ))
                                        |> (\content ->
                                                div [ class "edit-item middle" ]
                                                    [ content
                                                    , toolsOf (Middle idx) v name
                                                    ]
                                           )
                            )

                appendButton =
                    div [ class "append-middle" ] [ widget (Big <| Button.Append "middle name") AppendMiddle ]

                lastHtml =
                    div [ class "edit-item last" ]
                        [ Block.view last |> Html.map (\v -> SubMsg ( Last, v ))
                        , toolsOf Last last name
                        ]
            in
            div [ class "primary" ]
                [ div [ class "editing" ]
                    [ div [ class "edit-items" ]
                        (List.concat [ [ firstHtml ], middleHtml, [ appendButton ], [ lastHtml ] ])
                    , elideUnless (all Block.isFixed (first :: last :: middle))
                        (div [ class "confirm-edit" ])
                        [ Just (globalConfirm ())
                        , optionally (isEditing global) globalUndo
                        ]
                    ]
                ]

        _ ->
            log "Invalid state for view, not rendering anything!!!" noOp


newName : Model
newName =
    { global = BeingCreated
    , data =
        Editable
            { first = Block.Creating ( { kind = Block.First, allowed = [] }, "" )
            , middle = []
            , last = Block.Creating ( { kind = Block.Surname, allowed = [] }, "" )
            }
    , overrideMiddleValues = False
    }



--main : Program Basics.Never Model Msg
--main =
--    beginnerProgram
--        { model = newName
--        , update = update
--        , view = view
--        }
