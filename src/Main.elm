import Name
import Gender
import Interval
import Lib
import Html exposing (div, span, text, Html, program)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)

-- MODEL

type alias Card =
  { name : Name.Model
  , gender : Gender.Model
  , birthdeath : Interval.Model
  }

type alias Model = Card

-- UPDATE

type Msg =
  NameMsg Name.Msg
  | GenderMsg Gender.Msg
  | BirthDeathMsg Interval.Msg

update : Msg -> Model -> Model
update msg model =
  case msg of
    NameMsg nameMsg ->
      {model | name = Name.update nameMsg model.name}
    GenderMsg genderMsg ->
      {model | gender = Gender.update genderMsg model.gender}
    BirthDeathMsg msg ->
      {model | birthdeath = Interval.update msg model.birthdeath}

view : Model -> Html Msg
view model =
  case model.gender of
    Nothing ->
      div [class "namecard"]
      [ Html.map GenderMsg (Gender.view model.gender) ]
    _ ->
      let
        genderClass = Gender.stringify model.gender
      in
        div [class ("namecard " ++ genderClass)]
          [ div [class "main-info"]
            [ div [class ("no-portrait fas fa-" ++ genderClass ++ " fa-2x fa-fw fa-border fa-pull-left"), title "Add portrait"] []
            , div [class "summary"]
              [ Html.map NameMsg (Name.view model.name)
              , Html.map BirthDeathMsg (BirthDeath.view model.birthdeath)
              , div [class "dataline birth-death"]
                [ Html.map BirthDeathMsg (Interval.view model.birthdeath) ]
              ]
            ]
          , div [class "expander", title "More info"]
            [ div [class "expand fas fa-chevron-down"] [] ]
          , span [onClick (NameMsg Name.AddNickname)] [text "ADD NICKNAME"]
          ]

newCard : Model
newCard =
  { name = Name.newName
  , gender = Nothing
  , birthdeath = Interval.newInterval "date of birth" "date of death"
  }

main : Program Basics.Never Model Msg
main =
  program
    { init = (newCard, Cmd.none)
    , update = Lib.complexify update
    , subscriptions = \_ -> Sub.none
    , view = view
    }
