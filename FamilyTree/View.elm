module FamilyTree.View exposing (view)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
--import WebSocket
import FamilyTree.Basic exposing (..)
import Bootstrap.Modal exposing (makeModal)
import DataTypes exposing (..)
import Components.Name.Primary as PrimaryName

-- VIEW

view : Model -> Html Message
view model =
  let
    contentFunc =
      case model.tree.nodes of
        [] -> brandNewTree
        nodes -> \() -> existingTree model
    modalContent =
      case model.appState of
        Default -> Html.map PrimaryNameMessage (PrimaryName.view model.primaryModel)
        CreatedModal ->
          let
            content = span [] [Html.map PrimaryNameMessage (PrimaryName.view model.primaryModel)]
            cancel = {text="Cancel", class="btn btn-default", message=ModalCancelled}
            create = {text="Create", class="btn btn-primary btn-default disabled", message=NoOp}
          in
            makeModal {id="create-modal", title="Create person", label="Create a new person", content=content, buttons=[create,cancel]}
        AllowCreation ->
          let
            content = span [] [text "Hello, world!"]
            cancel = {text="Cancel", class="btn btn-default", message=ModalCancelled}
            testPerson = { id="00000000", name=[(Nickname "Rufus", Nothing)], birth={date=Nothing,place=Nothing}, death=Nothing, gender=Male, events=[], attributes=[] }
            create = {text="Create", class="btn btn-primary btn-default disabled", message=ProceedWithCreation testPerson}
          in
            makeModal {id="create-modal", title="Create person", label="Create a new person", content=content, buttons=[create,cancel]}
  in
    div []
    [ modalContent
    , nav [class "navbar navbar-inverse navbar-fixed-top", attribute "role" "navigation"]
      [ div [class "container"]
        [ div [class "navbar-header"]
          [ button [type_ "button", class "navbar-toggle collapsed", attribute "data-toggle" "collapse", attribute "data-target" "#navbar", attribute "aria-expanded" "false", attribute "aria-controls" "navbar"]
            [ span [class "sr-only"] [text "Toggle navigation"]
            , span [class "icon-bar"] []
            ]
            , a [class "navbar-brand", href "#"] [text "Family tree"]
          ]
        ]
      ]
    , div [class "jumbotron jumbotron-fluid"]
      [ div [class "container"] [contentFunc ()] ]
    , hr [] []
    , footer [(class "wrapper")]
        [ p [] [text "Legalese goes here, I guess..."] ]
    ]

brandNewTree : () -> Html Message
brandNewTree () =
  div [class "wrapper", style [("margin", "auto"), ("display", "block"), ("text-align", "center")]]
  [ h3 [] [text "This family tree is empty!"]
  , p [] [text "Click on the button below to add a family member to this tree."]
  , a [class "btn btn-primary btn-lg", href "#", attribute "role" "button", onClick CreateButtonClick] [ text "Start your family tree" ]
  ]

existingTree : Model -> Html Message
existingTree model =
  div [] [text "Nothing here either!! :-("]
