module Interval exposing (Model, Msg, knownEnd, knownStart, newInterval, update, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)
import SingleDate exposing (Msg(..), toJulian)


type alias Model =
    { start : SingleDate.Model
    , end : SingleDate.Model
    , startText : String
    , endText : String
    }


type Msg
    = Start SingleDate.Msg
    | End SingleDate.Msg


knownEnd : Model -> Bool
knownEnd model =
    not <| SingleDate.isUnspecified model.end


knownStart : Model -> Bool
knownStart model =
    not <| SingleDate.isUnspecified model.start


update : Msg -> Model -> ( Model, Cmd Msg )
update received model =
    case ( received, toJulian model.start, toJulian model.end ) of
        ( Start msg, _, Just limit ) ->
            let
                ( updated, c ) =
                    SingleDate.update msg model.start

                ( updated_, c_ ) =
                    case toJulian updated of
                        Nothing ->
                            ( updated, c )

                        Just proposed ->
                            if proposed <= limit then
                                ( updated, c )

                            else
                                SingleDate.update (ForceBadRange ("The " ++ model.startText ++ " is too far in the future")) updated

                ( endUpdate, endC ) =
                    SingleDate.update Revalidate model.end
            in
            ( { model | start = updated_, end = endUpdate }
            , Cmd.batch [ Cmd.map Start c_, Cmd.map End endC ]
            )

        ( Start msg, _, Nothing ) ->
            let
                ( updated, c ) =
                    SingleDate.update msg model.start
            in
            ( { model | start = updated }
            , Cmd.map Start c
            )

        ( End msg, Just limit, _ ) ->
            let
                ( updated, c ) =
                    SingleDate.update msg model.end

                ( updated_, c_ ) =
                    case toJulian updated of
                        Nothing ->
                            ( updated, c )

                        Just proposed ->
                            if proposed >= limit then
                                ( updated, c )

                            else
                                SingleDate.update (ForceBadRange ("The " ++ model.endText ++ " is too far in the past")) updated

                ( startUpdate, startC ) =
                    SingleDate.update Revalidate model.start
            in
            ( { model | end = updated_, start = startUpdate }
            , Cmd.batch [ Cmd.map End c_, Cmd.map Start startC ]
            )

        ( End msg, Nothing, _ ) ->
            let
                ( updated, c ) =
                    SingleDate.update msg model.end
            in
            ( { model | end = updated }
            , Cmd.map End c
            )


view : Model -> Html Msg
view model =
    let
        dateTitle model_ txt =
            if SingleDate.isUnspecified model_ then
                "Add " ++ txt

            else
                "Change " ++ txt

        dateAttrs model_ f txt =
            if SingleDate.isFixed model_ then
                [ onClick (f Edit), title (dateTitle model_ txt) ]

            else
                []

        startAttrs =
            dateAttrs model.start Start model.startText

        endAttrs =
            dateAttrs model.end End model.endText
    in
    div [ class "interval" ]
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
