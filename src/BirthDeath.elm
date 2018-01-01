module BirthDeath exposing (Model, Msg, update, view, newBirthDeath)
import SingleDate
import Html exposing (div, Html, text)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)

type alias Model =
  { birth : SingleDate.Model
  , death : SingleDate.Model
  }

type Msg =
  Birth SingleDate.Msg
  | Death SingleDate.Msg

update : Msg -> Model -> Model
update msg model =
  case msg of
    Birth msg -> {model | birth = SingleDate.simpleUpdate msg model.birth}
    Death msg -> {model | death = SingleDate.simpleUpdate msg model.death}

view : Model -> Html Msg
view model =
  let
    dateTitle model txt =
      if SingleDate.isUnspecified model then
        "Add " ++ txt
      else "Change " ++ txt
    dateAttrs model f txt =
      if SingleDate.isFixed model then
        [ onClick (f SingleDate.Edit), title (dateTitle model txt) ]
      else []
    birthAttrs = dateAttrs model.birth Birth "date of birth"
    deathAttrs = dateAttrs model.death Death "date of death"
  in
    div [class "dataline"]
    [ div [class "dates"]
      [ div [class "minmax"]
        [ div (class "birth" :: birthAttrs)
          [ Html.map Birth (SingleDate.view model.birth) ]
        , text " — "
        , div (class "death" :: deathAttrs)
          [ Html.map Death (SingleDate.view model.death) ]
        ]
      ]
    ]

newBirthDeath : Model
newBirthDeath = {birth = SingleDate.newDate, death = SingleDate.newDate}
