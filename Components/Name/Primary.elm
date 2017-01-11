module Components.Name.Primary exposing (update, view, Model, initialModel, Message(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Bootstrap.Glyphicon exposing (..)

-- ASSUMPTIONS:
-- 1. We are in a <div class="form-horizontal">
-- 2. The element in (1) is contained in a <div class="container">

-- Model

type alias PrimaryName =
  { first : String
  , middle : List String
  , last : Maybe String
  }

type Message =
  AddMiddleName
  | RemoveMiddleName Int
  | AddLastName
  | RemoveLastName
  | ChangeFirstName String
  | ChangeLastName String
  | ChangeMiddleName (Int, String)

type alias Model = PrimaryName

initialModel : Model
initialModel = { first = "", middle = [], last = Nothing }

-- Update

update : Message -> Model -> (Model, Cmd Message)
update action model =
  case action of
    AddMiddleName -> ({model | middle=""::model.middle}, Cmd.none)
    RemoveMiddleName i ->
      let newList = List.take i model.middle ++ List.drop (i+1) model.middle
      in ({model | middle=newList}, Cmd.none)
    AddLastName -> ({model | last=Just ""}, Cmd.none)
    RemoveLastName -> ({model | last=Nothing}, Cmd.none)
    ChangeFirstName s -> ({model | first=s}, Cmd.none)
    ChangeLastName s -> ({model | last=Just s}, Cmd.none)
    ChangeMiddleName (i,s) ->
      let newList = List.take i model.middle ++ (s :: List.drop (i+1) model.middle)
      in ({model | middle=newList}, Cmd.none)

-- View

humanCounter : Int -> String
humanCounter i =
  case i of
    1 -> "First"
    2 -> "Second"
    3 -> "Third"
    4 -> "Fourth"
    5 -> "Fifth"
    6 -> "Sixth"
    7 -> "Seventh"
    8 -> "Eighth"
    9 -> "Ninth"
    10 -> "Tenth"
    n -> if n < 1 then "" else "Nth"

middleNameRow : Int -> String -> Html Message
middleNameRow idx txt =
  div [classList [("col-sm-10",True), ("col-sm-offset-2", idx > 0)]]
    [ div [class "input-group"]
      [ input [type_ "text", id ("middle-name-" ++ toString idx), class "form-control", placeholder "Middle name", attribute "aria-describedby" "middle-name-label", onInput (\s -> ChangeMiddleName (idx, s))] [text txt]
      , span [class "input-group-btn"]
        [button [type_ "button", class "btn btn-default", onClick (RemoveMiddleName idx)] [icon MinusSign "Remove this name"]]
      ]
    ]

addButton : Message -> String -> Html Message
addButton action txt =
  button [type_ "button", class "btn btn-default btn-sm", onClick action] [iconText PlusSign txt]

indented : List (Html Message) -> Html Message
indented content =
  div [class "col-sm-offset-2 col-sm-10"] content

formGroup : List (Html Message) -> Html Message
formGroup content =
  div [class "form-group"] content

emptyMiddleContent : Html Message
emptyMiddleContent = addButton AddMiddleName "Add middle name"

emptyLastContent : Html Message
emptyLastContent = addButton AddLastName "Add last name"

middleContent : List String -> Html Message
middleContent middle =
  case middle of
    [] -> formGroup [indented [emptyMiddleContent]]
    x::xs -> -- more than one middle name
      let
        firstEntry =
          formGroup
            [ label [for "middle-name-0", id "middle-name-label", class "col-sm-2 control-label"]
              [text (if List.length middle == 1 then "Middle name" else "Middle names")]
            , middleNameRow 0 x
            ]
        nextEntries =
          middle |> List.drop 1 |> List.indexedMap (\idx -> \txt ->
            formGroup [middleNameRow (idx+1) txt]
          )
      in
        div [] (firstEntry :: nextEntries)

lastContent : Maybe String -> Html Message
lastContent last =
  case last of
    Nothing -> formGroup [indented [emptyLastContent]]
    Just last ->
      formGroup
        [ label [for "last-name", id "last-name-label", class "col-sm-2 control-label"] [text "Last name"]
        , div [class "col-sm-10"]
          [ div [class "input-group"]
            [ input [type_ "text", id "last-name", class "form-control", placeholder "Last name", attribute "aria-describedby" "last-name-label", onInput ChangeLastName] [text last]
            , span [class "input-group-btn"]
              [button [type_ "button", class "btn btn-default", onClick RemoveLastName] [icon MinusSign "Remove last name"]]
            ]
          ]
        ]

firstContent : String -> Html Message
firstContent name =
  formGroup
    [ label [for "first-name", id "first-name-label", class "col-sm-2 control-label"] [text "First name"]
    , div [class "col-sm-10"]
      [ input [type_ "text", id "first-name", class "form-control", placeholder "First name", attribute "aria-describedby" "first-name-label", onInput ChangeFirstName]
        [text name]
      ]
    ]

view : Model -> Html Message
view model =
  case (model.middle, model.last) of
    ([], Nothing) ->
      div []
        [ firstContent model.first
        , formGroup [indented [emptyMiddleContent, emptyLastContent]]
        ]
    ([], Just last) ->
      div []
        [ firstContent model.first
        , formGroup [indented [emptyMiddleContent]]
        , lastContent model.last
        ]
    (middle, Nothing) ->
      div []
        [ firstContent model.first
        , middleContent model.middle
        , formGroup [indented [addButton AddMiddleName "Add another middle name", emptyLastContent]]
        ]
    _ ->
      div []
        [ firstContent model.first
        , middleContent model.middle -- middle name
        , lastContent model.last -- last name
        ]
