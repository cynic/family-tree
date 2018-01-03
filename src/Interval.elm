module Interval exposing (Model, Msg, update, view, newInterval)
import SingleDate
import Html exposing (div, Html, text)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)

type alias Model =
  { start : SingleDate.Model
  , end : SingleDate.Model
  , startText : String
  , endText : String
  }

type Msg =
  Start SingleDate.Msg
  | End SingleDate.Msg

update : Msg -> Model -> Model
update msg model =
  case msg of
    Start msg -> {model | start = SingleDate.simpleUpdate msg model.start}
    End msg -> {model | end = SingleDate.simpleUpdate msg model.end}

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
    startAttrs = dateAttrs model.start Start model.startText
    endAttrs = dateAttrs model.end End model.endText
  in
    div [class "interval"]
    [ div (class "start" :: startAttrs)
      [ Html.map Start (SingleDate.view model.start) ]
    , text " — "
    , div (class "end" :: endAttrs)
      [ Html.map End (SingleDate.view model.end) ]
    ]

newInterval : String -> String -> Model
newInterval txtStart txtEnd =
  { start = SingleDate.newDate
  , end = SingleDate.newDate
  , startText = txtStart
  , endText = txtEnd
  }
