import Name
import Gender
import Interval
import Location
import StringValue
import Lib exposing (someHtml, optionally, elideUnless, elideIf)
import Html exposing (div, span, text, Html, program, ul, li)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)

-- MODEL

type alias Card =
  { name : Name.Model
  , gender : Gender.Model
  , birthdeath : Interval.Model
  , location : Location.Model
  , expanded : Bool
  }

type alias Model = Card

-- UPDATE

type Msg =
  NameMsg Name.Msg
  | GenderMsg Gender.Msg
  | BirthDeathMsg Interval.Msg
  | LocationMsg Location.Msg
  | ToggleExpand

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NameMsg nameMsg ->
      {model | name = Name.update nameMsg model.name} ! []
    GenderMsg genderMsg ->
      {model | gender = Gender.update genderMsg model.gender} ! []
    BirthDeathMsg msg ->
      Interval.update msg model.birthdeath
      |> \(v,c) -> {model | birthdeath = v} ! [Cmd.map BirthDeathMsg c]
    LocationMsg msg ->
      {model | location=Location.update msg model.location} ! []
    ToggleExpand ->
      {model | expanded = not model.expanded } ! []

view : Model -> Html Msg
view model =
  case model.gender of
    Nothing ->
      div [class "namecard"]
      [ Html.map GenderMsg (Gender.view model.gender) ]
    _ ->
      let
        genderClass = Gender.stringify model.gender
        expandedInfo =
          [ elideIf (model.location == StringValue.Gone) (div [class "dataline location"])
            [ Just <| Html.map LocationMsg (Location.view model.location) ]
          , someHtml (ul [class "namecard-actions fa-ul"])
            [ Just <| li [class "action"]
              [ span [class "fas fa-li fa-transgender"] []
              , span [class "action-item", onClick (GenderMsg Gender.Switch)]
                [text "Change gender"]
              ]
            , optionally (List.isEmpty model.name.nicknames)
              ( \() -> li [class "action"]
                [ span [class "fas fa-li fa-hand-point-right"] []
                , span [class "action-item", onClick (NameMsg Name.AddNickname)]
                  [text "Add nickname"]
                ]
              )
            , optionally (Interval.knownEnd model.birthdeath)
              ( \() -> li [class "action"]
                [ span [class "fas fa-li fa-book"] []
                , span [class "action-item"] [text "Add burial plot"]
                ]
              )
            , optionally (model.location == StringValue.Gone)
              ( \() -> li [class "action"]
                [ span [class "fas fa-li fa-map-marker"] []
                , span [class "action-item", onClick (LocationMsg (StringValue.Create [StringValue.CanDelete]))]
                  [text "Add location"]
                ]
              )
            ]
          ]
      in
        div [class ("namecard " ++ genderClass)]
          [ div [class "main-info"]
            [ div [class ("no-portrait fas fa-" ++ genderClass ++ " fa-2x fa-fw fa-border fa-pull-left"), title "Add portrait"] []
            , div [class "summary"]
              [ div [class "dataline name"]
                [ Html.map NameMsg (Name.view model.name) ]
              , div [class "dataline birth-death"]
                [ Html.map BirthDeathMsg (Interval.view model.birthdeath) ]
              ]
            ]
          , elideUnless model.expanded (div [class "expanded-info"])
            (List.map Just expandedInfo)
          , div [class "expander", title (if model.expanded then "Less info" else "More info"), onClick ToggleExpand]
            [ div [class ("expand fas " ++ (if model.expanded then "fa-chevron-up" else "fa-chevron-down"))] []
            ]
          ]

newCard : Model
newCard =
  { name = Name.newName
  , gender = Nothing
  , birthdeath = Interval.newInterval "date of birth" "date of death"
  , location = StringValue.Gone
  , expanded = False
  }

main : Program Basics.Never Model Msg
main =
  program
    { init = (newCard, Cmd.none)
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }
