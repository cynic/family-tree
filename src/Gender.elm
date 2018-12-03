module Gender exposing (Model, Msg(..), stringify, update, view)

import Button exposing (Button(..))
import Debug exposing (log)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Lib exposing (..)



-- Model


type Gender
    = Male
    | Female


type alias Model =
    Maybe Gender


type Msg
    = Switch
    | Select Gender



-- Update


stringify : Model -> String
stringify model =
    case model of
        Nothing ->
            ""

        Just Male ->
            "male"

        Just Female ->
            "female"


update : Msg -> Model -> Model
update msg model =
    case ( model, msg ) of
        ( Nothing, Select x ) ->
            Just x

        ( Just Male, Switch ) ->
            Just Female

        ( Just Female, Switch ) ->
            Just Male

        _ ->
            log "Invalid update for Gender" ( model, msg )
                |> (\( m, _ ) -> m)



-- View


view : Model -> Html Msg
view model =
    case model of
        Nothing ->
            div [ class "gender" ]
                [ widget (Big Button.Male) (Select Male)
                , div [ class "divider" ] []
                , widget (Big Button.Female) (Select Female)
                ]

        Just _ ->
            noOp
