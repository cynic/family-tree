import PrimaryName
import Html exposing (div, Html, beginnerProgram)
import Html.Attributes exposing (class)

-- MODEL

type Gender = Male | Female

type alias Card =
  { name : PrimaryName.Model
  }

type alias Model = Card

-- UPDATE

type Msg =
  NameMsg PrimaryName.Msg

update : Msg -> Model -> Model
update msg model =
  case msg of
    NameMsg primaryMsg ->
      {model | name = PrimaryName.update primaryMsg model.name}

view : Model -> Html Msg
view model =
  div [class "namecard"]
    [ div [class "main-line"]
      [ div [class "image"] []
      , Html.map NameMsg (PrimaryName.view model.name)
      ]
    ]

main : Program Basics.Never Model Msg
main =
  let
    initial = {name = PrimaryName.newName}
  in
    beginnerProgram
      { model = initial
      , update = update
      , view = view
      }
