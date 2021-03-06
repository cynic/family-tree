module SingleDate exposing
    ( Limit
    , Model
    , Msg(..)
    , isFixed
    , isUnspecified
    , newDate
    , toJulian
    , update
    , view
    )

import Button exposing (Button(..))
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class, classList, maxlength, pattern, placeholder, size, title, value)
import Html.Events exposing (onInput)
import Lib exposing (..)
import Task
import Time exposing (Month)



-- Model


type Converted
    = Ok Int -- converted value
    | Invalid String -- error string
    | OutOfRange ( Int, String )
    | Empty -- nuthin' G



{-
   Commented tuples here, because Elm is presently unable to
   do nested destructuring of records :-/
-}


type alias EditingDate =
    ( Converted, Converted, Converted )



-- Y,M,D


type FixedDate
    = Unspecified
    | Year Int
    | YearMonth ( Int, Int )
    | YearMonthDay ( Int, Int, Int )


type alias Limit =
    { year : Int
    , month : Int
    , day : Int
    }


type alias JulianDay =
    Int


type State
    = Editing ( EditingDate, FixedDate )
    | Known FixedDate


type alias Model =
    { date : Time.Posix
    , state : State
    , maxima : Dict String JulianDay
    , minima : Dict String JulianDay
    }


type Which
    = Y
    | M
    | D


type Msg
    = Edit
    | Confirm
    | Cancel
    | Update ( Which, String )
    | ForceBadRange String -- reason
    | Revalidate
    | UpdateCurrentDate Time.Posix


isFixed : Model -> Bool
isFixed model =
    case model.state of
        Known _ ->
            True

        _ ->
            False


isUnspecified : Model -> Bool
isUnspecified model =
    case model.state of
        Known Unspecified ->
            True

        _ ->
            False


daysInMonth : Converted -> Converted -> Int
daysInMonth y m =
    case ( m, y ) of
        ( Ok 1, _ ) ->
            31

        ( Ok 2, Ok y_ ) ->
            let
                isLeap =
                    (modBy 4 y_ == 0 && (modBy 100 y_ > 0)) || modBy 400 y_ == 0
            in
            if isLeap then
                29

            else
                28

        ( Ok 2, _ ) ->
            29

        -- largest possible! will reevaluate before confirm.
        ( Ok 3, _ ) ->
            31

        ( Ok 4, _ ) ->
            30

        ( Ok 5, _ ) ->
            31

        ( Ok 6, _ ) ->
            30

        ( Ok 7, _ ) ->
            31

        ( Ok 8, _ ) ->
            31

        ( Ok 9, _ ) ->
            30

        ( Ok 10, _ ) ->
            31

        ( Ok 11, _ ) ->
            30

        ( Ok 12, _ ) ->
            31

        _ ->
            31



-- largest possible! will reevaluate before confirm.
-- thanks to https://quasar.as.utexas.edu/BillInfo/JulianDatesG.html


julianDay : Limit -> JulianDay
julianDay { year, month, day } =
    let
        y =
            if month == 1 || month == 2 then
                year - 1

            else
                year

        m =
            if month == 1 || month == 2 then
                month + 12

            else
                month

        d =
            day

        a =
            truncate (toFloat y / 100)

        b =
            truncate (toFloat a / 4)

        c =
            2 - a + b

        e =
            truncate (365.25 * (toFloat y + 4716))

        f =
            truncate (30.6001 * (1 + toFloat m))
    in
    c + d + e + f - 1525


valueOf : Converted -> Maybe Int
valueOf v =
    case v of
        Ok val ->
            Just val

        OutOfRange ( val, _ ) ->
            Just val

        _ ->
            Nothing


editingToLimit : EditingDate -> Maybe Limit
editingToLimit ( y, m, d ) =
    case ( valueOf y, valueOf m, valueOf d ) of
        ( Just y_, Nothing, Nothing ) ->
            Just { year = y_, month = 1, day = 1 }

        ( Just y_, Just m_, Nothing ) ->
            Just { year = y_, month = m_, day = 1 }

        ( Just y_, Just m_, Just d_ ) ->
            Just { year = y_, month = m_, day = d_ }

        _ ->
            Nothing


toLimit : State -> Maybe Limit
toLimit state =
    case state of
        Known Unspecified ->
            Nothing

        Known (Year y) ->
            Just { year = y, month = 1, day = 1 }

        Known (YearMonth ( y, m )) ->
            Just { year = y, month = m, day = 1 }

        Known (YearMonthDay ( y, m, d )) ->
            Just { year = y, month = m, day = d }

        Editing ( v, _ ) ->
            editingToLimit v


toJulian : Model -> Maybe JulianDay
toJulian { state } =
    case toLimit state of
        Just limit ->
            Just <| julianDay limit

        Nothing ->
            Nothing


monthToInt : Time.Month -> Int
monthToInt x =
    case x of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        update_ v_ max =
            case valueOf v_ of
                Just val ->
                    if val < 1 then
                        OutOfRange ( val, "This value cannot be less than 1" )

                    else if val > max then
                        OutOfRange ( val, "This value cannot be greater than " ++ String.fromInt max )

                    else
                        Ok val

                -- it's in-range!
                Nothing ->
                    v_
    in
    case ( msg, model.state ) of
        ( UpdateCurrentDate d, _ ) ->
            ( { model | date = d }, Cmd.none )

        ( Edit, Known f ) ->
            let
                editInit =
                    case f of
                        Unspecified ->
                            ( Empty, Empty, Empty )

                        Year y ->
                            ( Ok y, Empty, Empty )

                        YearMonth ( y, m ) ->
                            ( Ok y, Ok m, Empty )

                        YearMonthDay ( y, m, d ) ->
                            ( Ok y, Ok m, Ok d )
            in
            ( { model | state = Editing ( editInit, f ) }, Task.perform UpdateCurrentDate Time.now )

        ( Confirm, Editing ( conv, _ ) ) ->
            let
                fixedInit =
                    case conv of
                        ( Ok y, Ok m, Ok d ) ->
                            YearMonthDay ( y, m, d )

                        ( Ok y, Ok m, _ ) ->
                            YearMonth ( y, m )

                        ( Ok y, _, _ ) ->
                            Year y

                        _ ->
                            Unspecified
            in
            ( { model | state = Known fixedInit }, Cmd.none )

        ( Cancel, Editing ( _, f ) ) ->
            ( { model | state = Known f }, Cmd.none )

        ( Update ( which, upd ), Editing ( ( y, m, d ), f ) ) ->
            let
                parsed =
                    case String.toInt upd of
                        Just v ->
                            Ok v

                        Nothing ->
                            if String.isEmpty (String.trim upd) then
                                Empty

                            else
                                Invalid "This is not an integer"

                updatedY =
                    if which == Y then
                        update_ parsed (Time.toYear Time.utc model.date)

                    else
                        update_ y (Time.toYear Time.utc model.date)

                updatedM =
                    if which == M then
                        update_ parsed 12

                    else
                        update_ m 12

                updatedD =
                    if which == D then
                        update_ parsed (daysInMonth updatedY updatedM)

                    else
                        update_ d (daysInMonth updatedY updatedM)

                new_state =
                    Editing ( ( updatedY, updatedM, updatedD ), f )
            in
            ( { model | state = new_state }, Cmd.none )

        ( Revalidate, Editing ( ( y, m, d ), f ) ) ->
            let
                updatedY =
                    update_ y (Time.toYear Time.utc model.date)

                updatedM =
                    update_ m 12

                updatedD =
                    update_ d (daysInMonth updatedY updatedM)

                new_state =
                    Editing ( ( updatedY, updatedM, updatedD ), f )
            in
            ( { model | state = new_state }, Cmd.none )

        ( Revalidate, Known _ ) ->
            ( model, Cmd.none )

        -- no revalidation necessary, known is known.
        ( ForceBadRange reason, Editing ( ( y, m, d ), f ) ) ->
            let
                toOOR v =
                    case v of
                        OutOfRange ( v_, _ ) ->
                            OutOfRange ( v_, reason )

                        Ok v_ ->
                            OutOfRange ( v_, reason )

                        Invalid _ ->
                            v

                        Empty ->
                            Empty

                new_state =
                    Editing ( ( toOOR y, toOOR m, toOOR d ), f )
            in
            ( { model | state = new_state }, Cmd.none )

        ( ForceBadRange _, Known _ ) ->
            ( model, Cmd.none )

        -- Can't force a bad range onto a known date.
        _ ->
            log "Invalid transition in SingleDate" ( model, msg )
                |> (\( m, _ ) -> ( m, Cmd.none ))


monthString : Int -> String
monthString v =
    case v of
        1 ->
            "January"

        2 ->
            "February"

        3 ->
            "March"

        4 ->
            "April"

        5 ->
            "May"

        6 ->
            "June"

        7 ->
            "July"

        8 ->
            "August"

        9 ->
            "September"

        10 ->
            "October"

        11 ->
            "November"

        12 ->
            "December"

        _ ->
            ""


stringifyConverted : Converted -> String
stringifyConverted value =
    case valueOf value of
        Just n ->
            String.fromInt n

        Nothing ->
            ""


stringifyFixed : FixedDate -> String
stringifyFixed value =
    case value of
        Unspecified ->
            "??"

        Year y ->
            String.fromInt y

        YearMonth ( y, m ) ->
            monthString m ++ " " ++ String.fromInt y

        YearMonthDay ( y, m, d ) ->
            monthString m ++ " " ++ String.fromInt d ++ ", " ++ String.fromInt y


isOk : Converted -> Bool
isOk conv =
    case conv of
        Ok _ ->
            True

        _ ->
            False


canConfirm : ( Converted, Converted, Converted ) -> Bool
canConfirm ( y, m, d ) =
    let
        empty b =
            b == Empty

        confirmable b =
            isOk b

        ok_1 =
            confirmable y && empty m && empty d

        ok_2 =
            confirmable y && confirmable m && empty d

        ok_3 =
            confirmable y && confirmable m && confirmable d
    in
    ok_1 || ok_2 || ok_3


isError : Converted -> Bool
isError conv =
    case conv of
        Invalid _ ->
            True

        OutOfRange _ ->
            True

        _ ->
            False


errorMessage : Converted -> String
errorMessage conv =
    case conv of
        Invalid v ->
            v

        OutOfRange ( _, v ) ->
            v

        _ ->
            ""


view : Model -> Html Msg
view model =
    case model.state of
        Editing ( editableDate, _ ) ->
            let
                ( y, m, d ) =
                    editableDate
            in
            div [ class "date" ]
                [ div [ class "editing" ]
                    [ div [ class "year" ]
                        [ input
                            [ size 5
                            , placeholder "yyyy"
                            , maxlength 4
                            , pattern "\\d{1,4}"
                            , value (stringifyConverted y)
                            , onInput (\v -> Update ( Y, v ))
                            , classList [ ( "invalid", isError y ) ]
                            ]
                            []
                        , elideUnless (isError y)
                            (div [ class "error", title (errorMessage y) ])
                            [ Just <| span [ class "fa fa-exclamation-triangle" ] [] ]
                        ]
                    , div [ class "month" ]
                        [ input
                            [ size 3
                            , placeholder "mm"
                            , maxlength 2
                            , pattern "\\d{1,2}"
                            , value (stringifyConverted m)
                            , onInput (\v -> Update ( M, v ))
                            , classList [ ( "invalid", isError m ) ]
                            ]
                            []
                        , elideUnless (isError m)
                            (div [ class "error", title (errorMessage m) ])
                            [ Just <| span [ class "fa fa-exclamation-triangle" ] [] ]
                        , elideUnless (isOk m)
                            (div [ class "description" ])
                            [ Just (text <| monthString (Maybe.withDefault 0 (valueOf m))) ]
                        ]
                    , div [ class "day" ]
                        [ input
                            [ size 3
                            , placeholder "dd"
                            , maxlength 2
                            , pattern "\\d{1,2}"
                            , value (stringifyConverted d)
                            , onInput (\v -> Update ( D, v ))
                            , classList [ ( "invalid", isError d ) ]
                            ]
                            []
                        , elideUnless (isError d)
                            (div [ class "error", title (errorMessage d) ])
                            [ Just <| span [ class "fa fa-exclamation-triangle" ] [] ]
                        ]

                    -- now for a "confirm" button
                    , elideUnless (canConfirm editableDate) (\_ -> widget (Big Button.Confirm) Confirm) []
                    , widget (Big Button.Cancel) Cancel
                    ]
                ]

        Known v ->
            div [ class "date" ]
                [ div [ class "fixed" ]
                    [ text (stringifyFixed v) ]
                ]


newDate : Model
newDate =
    { state = Known Unspecified
    , date = Time.millisToPosix 0
    , maxima = Dict.empty
    , minima = Dict.empty
    }
