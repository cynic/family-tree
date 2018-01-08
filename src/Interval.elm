module Interval exposing (Model, Msg, update, view, newInterval)
import SingleDate exposing (Msg(..), toJulian)
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (Debug.log "Interval msg" msg, toJulian model.start, toJulian model.end) of
    (Start msg, _, Just limit) ->
      let
        (updated, c) = SingleDate.update msg model.start
        (updated_, c_) =
          case toJulian updated of
            Nothing -> (updated, c)
            Just proposed ->
              if proposed <= limit then
                (updated, c)
              else
                SingleDate.update (ForceBadRange ("The " ++ model.startText ++ " is too far in the future")) updated
        (endUpdate, endC) =
          SingleDate.update Revalidate model.end
      in

        {model | start=updated_, end=endUpdate} ! [Cmd.map Start c_, Cmd.map End endC]
    (Start msg, _, Nothing) ->
      let
        (updated, c) = SingleDate.update msg model.start
      in
        {model | start = updated} ! [Cmd.map Start c]
    (End msg, Just limit, _) ->
      let
        (updated, c) = SingleDate.update msg model.end
        (updated_, c_) =
          case toJulian updated of
            Nothing -> (updated, c)
            Just proposed ->
              if proposed >= limit then
                (updated, c)
              else
                SingleDate.update (ForceBadRange ("The " ++ model.endText ++ " is too far in the past")) updated
        (startUpdate, startC) =
          SingleDate.update Revalidate model.start
      in
        {model | end=updated_, start=startUpdate} ! [Cmd.map End c_, Cmd.map Start startC]
    (End msg, Nothing, _) ->
      let
        (updated, c) = SingleDate.update msg model.end
      in
        {model | end = updated} ! [Cmd.map End c]

view : Model -> Html Msg
view model =
  let
    dateTitle model txt =
      if SingleDate.isUnspecified model then
        "Add " ++ txt
      else "Change " ++ txt
    dateAttrs model f txt =
      if SingleDate.isFixed model then
        [ onClick (f Edit), title (dateTitle model txt) ]
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
