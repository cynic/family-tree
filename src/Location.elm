module Location exposing (Model, Msg, newLocation, update, view)

--import Http exposing (encodeUri)

import Button
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, target, title)
import Html.Events exposing (onClick)
import Lib
import StringValue exposing (Config)
import Url.Builder exposing (crossOrigin, int, string)


type alias Model =
    StringValue.Model


type alias Msg =
    StringValue.Msg


config : StringValue.Config Msg
config =
    StringValue.Config
        { size = 14
        , description = "location"
        , toMsg = \x -> x
        , toolMap =
            [ ( StringValue.isEditing
              , \model ->
                    [ Lib.optionally (StringValue.canConfirm model) (\() -> Lib.widget Button.Confirm StringValue.Confirm)
                    , Just <| Lib.widget Button.Cancel StringValue.Cancel
                    , Just <| Lib.widget (Button.Delete "location") StringValue.Delete
                    ]
              )
            , ( StringValue.isCreating
              , \model ->
                    [ Lib.optionally (StringValue.canConfirm model) (\() -> Lib.widget Button.Confirm StringValue.Confirm)
                    , Just <| Lib.widget (Button.Delete "location") StringValue.Delete
                    ]
              )
            ]
        , customHtml =
            [ ( StringValue.isFixed
              , \model ->
                    \tools ->
                        div []
                            [ text (StringValue.getFixed model |> Maybe.withDefault "ERROR - NAME IS NOT FIXED")
                            , a
                                [ href
                                    (StringValue.getFixed model
                                    -- https://www.google.com/maps/search/?api=1&parameters
                                        |> Maybe.map (\v -> crossOrigin "https://www.google.com" [ "maps", "search", "" ] [ int "api" 1, string "query" v ])
                                        |> Maybe.withDefault ""
                                    )
                                , target "_blank"
                                , title "View on Google Maps"
                                , class "fas fa-map-marker widget maps"
                                ]
                                []
                            , tools
                            ]
              )
            ]
        }


update : Msg -> Model -> Model
update msg model =
    StringValue.update msg model


view : Model -> Html Msg
view model =
    let
        keyAttrs =
            case model of
                StringValue.Fixed _ ->
                    [ title "Change location", onClick StringValue.Edit ]

                _ ->
                    []
    in
    div [ class "location" ]
        [ div (class "key" :: keyAttrs) [ text "Location" ]
        , div [ class "data" ]
            [ StringValue.view config model ]
        ]


newLocation : Model
newLocation =
    StringValue.Creating ( [ StringValue.CanDelete ], "" )
