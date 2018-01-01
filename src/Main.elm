import Name
import Gender
import BirthDeath
import Lib
import Html exposing (div, span, text, Html, program)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)

-- MODEL

type alias Card =
  { name : Name.Model
  , gender : Gender.Model
  , birthdeath : BirthDeath.Model
  }

type alias Model = Card

-- UPDATE

type Msg =
  NameMsg Name.Msg
  | GenderMsg Gender.Msg
  | BirthDeathMsg BirthDeath.Msg

update : Msg -> Model -> Model
update msg model =
  case msg of
    NameMsg nameMsg ->
      {model | name = Name.update nameMsg model.name}
    GenderMsg genderMsg ->
      {model | gender = Gender.update genderMsg model.gender}
    BirthDeathMsg msg ->
      {model | birthdeath = BirthDeath.update msg model.birthdeath}

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
  , birthdeath = BirthDeath.newBirthDeath
  }

main : Program Basics.Never Model Msg
main =
  program
    { init = (newCard, Cmd.none)
    , update = Lib.complexify update
    , subscriptions = \_ -> Sub.none
    , view = view
    }
