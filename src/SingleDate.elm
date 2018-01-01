module SingleDate exposing
  ( Model, Msg(Edit), simpleUpdate, view, newDate, isFixed, isUnspecified
  )
import Lib exposing (..)
import Task
import Date as D
import Debug exposing (crash)
import Button exposing (Button(Big,Huge))
import Html exposing (Html, div, text, input, span)
import Html.Attributes exposing (class, title, value, defaultValue, placeholder, maxlength, pattern, size, classList)
import Html.Events exposing (onInput)

-- Model

type Converted =
  Ok Int -- converted value
  | Invalid String -- error string
  | OutOfRange (Int, String)
  | Empty -- nuthin' G

{-
Commented tuples here, because Elm is presently unable to
do nested destructuring of records :-/
-}

type alias EditingDate = (Converted,Converted,Converted) -- Y,M,D

type FixedDate =
  Unspecified
  | Year Int
  | YearMonth (Int,Int)
  | YearMonthDay (Int,Int,Int)

type State =
  Editing (EditingDate, FixedDate)
  | Known FixedDate

type alias Model =
  { date : D.Date
  , state : State
  }

type Which = Y | M | D

type Msg =
  Edit
  | Confirm
  | Cancel
  | Update (Which, String)
  | UpdateCurrentDate D.Date

isFixed : Model -> Bool
isFixed model =
  case model.state of
    Known _ -> True
    _ -> False

isUnspecified : Model -> Bool
isUnspecified model =
  case model.state of
    Known Unspecified -> True
    _ -> False

daysInMonth : Converted -> Converted -> Int
daysInMonth y m =
  case (m, y) of
    (Ok 1, _) -> 31
    (Ok 2, Ok y) ->
      let
        isLeap =
          (y % 4 == 0 && (y % 100 > 0)) || y % 400 == 0
      in
        if isLeap then 29 else 28
    (Ok 2, _) -> 29 -- largest possible! will reevaluate before confirm.
    (Ok 3, _) -> 31
    (Ok 4, _) -> 30
    (Ok 5, _) -> 31
    (Ok 6, _) -> 30
    (Ok 7, _) -> 31
    (Ok 8, _) -> 31
    (Ok 9, _) -> 30
    (Ok 10, _) -> 31
    (Ok 11, _) -> 30
    (Ok 12, _) -> 31
    _ -> 31 -- largest possible! will reevaluate before confirm.


simpleUpdate : Msg -> Model -> Model
simpleUpdate = uncomplexify update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model.state) of
    (UpdateCurrentDate d, _) ->
      ({model | date = Debug.log "UPDATED!" d}, Cmd.none)
    (Edit, Known f) ->
      let
        editInit =
          case f of
            Unspecified -> (Empty, Empty, Empty)
            Year y -> (Ok y, Empty, Empty)
            YearMonth (y,m) -> (Ok y, Ok m, Empty)
            YearMonthDay (y,m,d) -> (Ok y, Ok m, Ok d)
      in
        ({model | state = Editing (editInit, f)}, Task.perform UpdateCurrentDate D.now)
    (Confirm, (Editing (conv,_))) ->
      let
        fixedInit =
          case conv of
            (Ok y, Ok m, Ok d) -> YearMonthDay (y,m,d)
            (Ok y, Ok m, _) -> YearMonth (y,m)
            (Ok y, _, _) -> Year y
            _ -> Unspecified
      in
        ({model | state = Known fixedInit}, Cmd.none)
    (Cancel, (Editing (_, f))) ->
      ({model | state = Known f}, Cmd.none)
    (Update (which, upd), (Editing ((y,m,d),f))) ->
      let
        parsed =
          case String.toInt upd of
            Result.Ok v -> Ok v
            Result.Err _ ->
              if String.isEmpty (String.trim upd) then Empty
              else Invalid "This is not an integer"
        update v max =
          let
            extracted =
              case v of
                Ok v -> Just v
                OutOfRange (v,_) -> Just v
                _ -> Nothing
          in
            case extracted of
              Just v ->
                if v < 1 then
                  OutOfRange (v, "This value cannot be less than 1")
                else
                  if v > max then
                    OutOfRange (v, "This value cannot be greater than " ++ toString max)
                  else
                    Ok v -- it's in-range!
              Nothing -> v
        result =
          case which of
            Y ->
              let
                updatedY = update parsed (D.year model.date)
                updatedD = update d (daysInMonth updatedY m)
              in
                (updatedY, m, updatedD)
            M ->
              let
                updatedM = update parsed 12
              in
                (y, updatedM, update d (daysInMonth y updatedM))
            D -> (y, m, update parsed (daysInMonth y m))
      in
        ({model | state=Editing (result,f)}, Cmd.none)
    _ -> crash "Invalid transition in SingleDate"

monthString : Int -> String
monthString v =
  case v of
    1 -> "January"
    2 -> "February"
    3 -> "March"
    4 -> "April"
    5 -> "May"
    6 -> "June"
    7 -> "July"
    8 -> "August"
    9 -> "September"
    10 -> "October"
    11 -> "November"
    12 -> "December"
    _ -> ""

stringifyConverted : Converted -> String
stringifyConverted value =
  case value of
    Ok n -> toString n
    _ -> ""

stringifyFixed : FixedDate -> String
stringifyFixed value =
  case value of
    Unspecified -> "??"
    Year y -> toString y
    YearMonth (y,m) -> monthString m ++ " " ++ toString y
    YearMonthDay (y,m,d) -> monthString m ++ " " ++ toString d ++ ", " ++ toString y

getValue : Converted -> Int
getValue conv =
  case conv of
    Ok v -> v
    _ -> 0xdeadbeef

isOk : Converted -> Bool
isOk conv =
  case conv of
    Ok _ -> True
    _ -> False

canConfirm : (Converted, Converted, Converted) -> Bool
canConfirm (y, m, d) =
  let
    empty b = b == Empty
    confirmable b = isOk b
    ok_1 = confirmable y && empty m && empty d
    ok_2 = confirmable y && confirmable m && empty d
    ok_3 = confirmable y && confirmable m && confirmable d
  in
    ok_1 || ok_2 || ok_3

isError : Converted -> Bool
isError conv =
  case conv of
    Invalid _ -> True
    OutOfRange _ -> True
    _ -> False

errorMessage : Converted -> String
errorMessage conv =
  case conv of
    Invalid v -> v
    OutOfRange (_,v) -> v
    _ -> ""

view : Model -> Html Msg
view model =
  case model.state of
    Editing (editableDate, _) ->
      let
        (y, m, d) = editableDate
      in
        div [class "date"]
        [ div [class "editing"]
          [ div [class "year"]
            [ input
              [ size 5
              , placeholder "yyyy"
              , maxlength 4
              , pattern "\\d{1,4}"
              , defaultValue (stringifyConverted y)
              , onInput (\v -> Update (Y, v))
              , classList [("invalid", isError y)]
              ] []
            , elideUnless (isError y) (div [class "error", title (errorMessage y)])
              [ Just <| span [class "fa fa-exclamation-triangle"] [] ]
            ]
          , div [class "month"]
            [ input
              [ size 3
              , placeholder "mm"
              , maxlength 2
              , pattern "\\d{1,2}"
              , defaultValue (stringifyConverted m)
              , onInput (\v -> Update (M, v))
              , classList [("invalid", isError m)]
              ] []
            , elideUnless (isError m) (div [class "error", title (errorMessage m)])
              [ Just <| span [class "fa fa-exclamation-triangle"] [] ]
            , elideUnless (isOk m) (div [class "description"])
              [ Just (text <| monthString (getValue m)) ]
            ]
          , div [class "day"]
            [ input
              [ size 3
              , placeholder "dd"
              , maxlength 2
              , pattern "\\d{1,2}"
              , defaultValue (stringifyConverted d)
              , onInput (\v -> Update (D, v))
              , classList [("invalid", isError d)]
              ] []
            , elideUnless (isError d) (div [class "error", title (errorMessage d)])
              [ Just <| span [class "fa fa-exclamation-triangle"] [] ]
            ]
          -- now for a "confirm" button
          , elideUnless (canConfirm editableDate) (\_ -> widget (Big Button.Confirm) Confirm) []
          , widget (Big Button.Cancel) Cancel
          ]
        ]
    Known v ->
      div [class "date"]
      [ div [class "fixed"]
        [ text (stringifyFixed v) ]
      ]

newDate : Model
newDate = {state = Known Unspecified, date = D.fromTime 0}
