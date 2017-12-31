module Gender exposing (Msg(..), Model, stringify, update, view)
import Lib exposing (..)
import Button exposing (Button(Big,Huge))
import Debug exposing (crash)
import Html exposing (Html, div)
import Html.Attributes exposing (class)

-- Model

type Gender = Male | Female

type alias Model = Maybe Gender

type Msg =
  Switch
  | Select Gender

-- Update

stringify : Model -> String
stringify model =
  case model of
    Nothing -> ""
    Just Male -> "male"
    Just Female -> "female"

update : Msg -> Model -> Model
update msg model =
  case (model, msg) of
    (Nothing, Select x) -> Just x
    (Just Male, Switch) -> Just Female
    (Just Female, Switch) -> Just Male
    _ -> crash "Invalid update for Gender"

-- View

view : Model -> Html Msg
view model =
  case model of
    Nothing ->
      div [class "gender"]
      [ widget (Big Button.Male) (Select Male)
      , div [class "divider"] []
      , widget (Big Button.Female) (Select Female)
      ]
    Just _ -> noOp
