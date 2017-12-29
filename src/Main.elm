import Name
import Html exposing (div, span, text, Html, beginnerProgram)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)

-- MODEL

type alias Card =
  { name : Name.Model
  }

type alias Model = Card

-- UPDATE

type Msg =
  NameMsg Name.Msg

update : Msg -> Model -> Model
update msg model =
  case msg of
    NameMsg nameMsg ->
      {model | name = Name.update nameMsg model.name}

view : Model -> Html Msg
view model =
  div [class "namecard"]
    [ div [class "main-info"]
      [ div [class "portrait"] []
      , div [class "summary"]
        [ Html.map NameMsg (Name.view model.name)
        ]
      ]
    , div [class "expander", title "More info"]
      [ div [class "expand fas fa-chevron-down"] [] ]
    , span [onClick (NameMsg Name.AddNickname)] [text "ADD NICKNAME"]
    ]

main : Program Basics.Never Model Msg
main =
  let
    initial = {name = Name.newName}
  in
    beginnerProgram
      { model = initial
      , update = update
      , view = view
      }
