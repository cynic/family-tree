import PrimaryName
import Nickname
import Html exposing (div, Html, beginnerProgram)
import Html.Attributes exposing (class)

-- MODEL

type Gender = Male | Female

type alias Card =
  { name : PrimaryName.Model
  , nick : Nickname.Model
  }

type alias Model = Card

-- UPDATE

type Msg =
  PrimaryMsg PrimaryName.Msg
  | NickMsg Nickname.Msg

update : Msg -> Model -> Model
update msg model =
  case msg of
    PrimaryMsg primaryMsg ->
      {model | name = PrimaryName.update primaryMsg model.name}
    NickMsg nickMsg ->
      {model | nick = Nickname.update nickMsg model.nick}

view : Model -> Html Msg
view model =
  div [class "namecard"]
    [ div [class "main-line"]
      [ div [class "image"] []
      , Html.map PrimaryMsg (PrimaryName.view model.name)
      , Html.map NickMsg (Nickname.view model.nick)
      ]
    ]

main : Program Basics.Never Model Msg
main =
  let
    initial = {name = PrimaryName.newName, nick = Nickname.newName}
  in
    beginnerProgram
      { model = initial
      , update = update
      , view = view
      }
