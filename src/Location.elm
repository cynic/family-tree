module Location exposing (Msg, Model, update, view, newLocation)
import StringValue exposing (Config)
import Html exposing (Html, div, span, text, a)
import Html.Attributes exposing (class, title, href, target)
import Html.Events exposing (onClick)
import Http exposing (encodeUri)
import Lib
import Button

type alias Model = StringValue.Model

type alias Msg = StringValue.Msg

config : StringValue.Config Msg
config =
  StringValue.Config
    { size=14
    , overrideValue=False
    , description="location"
    , toMsg = (\x -> x)
    , toolMap =
      [ ( StringValue.isEditing,
          \model -> [ Lib.optionally (StringValue.canConfirm model) (\() -> Lib.widget Button.Confirm StringValue.Confirm)
          , Just <| Lib.widget Button.Cancel StringValue.Cancel
          , Just <| Lib.widget (Button.Delete "location") StringValue.Delete
          ]
        )
      , ( StringValue.isCreating,
          \model -> [ Lib.optionally (StringValue.canConfirm model) (\() -> Lib.widget Button.Confirm StringValue.Confirm)
          , Just <| Lib.widget (Button.Delete "location") StringValue.Delete
          ]
        )
      ]
    , customHtml =
      [ ( StringValue.isFixed,
          \model -> \tools -> div []
          [ text (StringValue.getFixed model)
          , a
            [ href ("https://www.google.com/maps/search/?api=1&query=" ++ encodeUri (StringValue.getFixed model))
            , target "_blank"
            , title "View on Google Maps"
            , class "fas fa-map-marker widget maps"
            ] []
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
        StringValue.Fixed _ -> [title "Change location", onClick StringValue.Edit]
        _ -> []
  in
    div [class "location"]
    [ div (class "key" :: keyAttrs) [text "Location"]
    , div [class "data"]
      [ StringValue.view config model ]
    ]

newLocation : Model
newLocation = StringValue.Creating ([StringValue.CanDelete], "")
