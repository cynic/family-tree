module Nickname exposing
  ( Model, Msg, update, view, newName
  )
import Html exposing (div, span, Html, input, text, beginnerProgram, node)
import Html.Attributes exposing (class, classList, title, attribute, size, defaultValue)
import NameBlock as Block exposing (NameKind(..), NameOps(..))
import Html.Events exposing (onClick)
import Lib exposing (noOp, widget, someHtml, optionally)
import Button exposing (Button(Big,Huge))

-- MODEL

type alias Model = Block.Model

-- UPDATE

type alias Msg = Block.Msg

update : Msg -> Model -> Model
update msg model = Block.update msg model

view : Model -> Html Msg
view model =
  case model of
    Block.Fixed _ ->
      div [class "nickname"]
      [ div [class "fixed", title "Click to edit", onClick Block.Edit]
        [ span [class "formal-text"] [text "“"]
        , Block.view model False
        , span [class "formal-text"] [text "”"]
        ]
      ]
    Block.Gone -> noOp
    Block.Editing _ ->
      div [class "nickname"]
      [ div [class "editing"]
        [ Block.view model False
        , someHtml (div [class "tools"])
          [ optionally (Block.canConfirm model) (\() -> widget Button.Confirm Block.Confirm)
          , Just (widget Button.Cancel Block.Cancel)
          , Just (widget (Button.Delete "nickname") Block.Delete)
          ]
        ]
      ]
    Block.Creating _ ->
      div [class "nickname"]
      [ div [class "editing"]
        [ Block.view model False
        , someHtml (div [class "tools"])
          [ optionally (Block.canConfirm model) (\() -> widget Button.Confirm Block.Confirm)
          , Just (widget (Button.Delete "nickname") Block.Delete)
          ]
        ]
      ]

newName : Model
newName = Block.Creating ({kind=Block.Nickname, allowed=[CanDelete, CanCancel]}, "")

main : Program Basics.Never Model Msg
main =
  beginnerProgram
    { model = newName
    , update = update
    , view = view
    }
