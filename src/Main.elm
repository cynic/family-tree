import Html exposing (..)
import Html.Attributes exposing (class, id, classList, title)
import Html.Events exposing (onMouseOver, onMouseOut)
import Maybe exposing (withDefault)

-- MODEL

type FocusState =
  NoFocus
  | MouseOn PinpointPart
  | Editing PinpointPart

type alias Model =
  { first : Maybe String
  , middle : Maybe (List String)
  , last : Maybe String
  , focus : FocusState
  }

-- UPDATE

type AddOrEditPart =
  First String -- the replacement
  | Middle (Int, String) -- (the index to edit at or insert after, the replacement)
  | Last String -- the replacement

type PinpointPart =
  PinpointFirst
  | PinpointMiddle Int -- index
  | PinpointLast

type Msg =
  Edit AddOrEditPart
  | Add AddOrEditPart
  | Delete PinpointPart
  | FocusToEdit PinpointPart
  | MouseOver PinpointPart
  | NoMouseOver

update : Msg -> Model -> Model
update msg model =
  case msg of
    MouseOver part ->
      { model | focus = MouseOn part }
    NoMouseOver ->
      { model | focus = NoFocus }
    _ ->
      model

-- VIEW

htmlName : Maybe String -> List (Attribute Msg) -> FocusState -> PinpointPart -> String -> Html Msg
htmlName x attrs focus part defaultString =
  let
    withInFunc = onMouseOver (MouseOver part)
    withOutFunc = onMouseOut NoMouseOver
    moused = if focus == MouseOn part then [class "moused", title "Click to change"] else []
    myAttrs = withInFunc :: withOutFunc :: (moused ++ attrs)
    myText = x |> withDefault defaultString
  in
    span myAttrs [ text myText ]

view : Model -> Html Msg
view model =
  let
    middleMap =
      case model.middle of
        Just many ->
          List.indexedMap (\idx -> \x -> htmlName (Just x) [] model.focus (PinpointMiddle idx) "??!") many
          |> List.intersperse (text " ")
        Nothing ->
          [ htmlName Nothing [] model.focus (PinpointMiddle 0) "(no middle names)" ]
  in
    span []
      ( htmlName model.first [] model.focus PinpointFirst "(no first name)"
      :: text " "
      :: middleMap
      ++ [text " ", htmlName model.last [] model.focus PinpointLast "(no last name)"]
      )

main : Program Basics.Never Model Msg
main =
  beginnerProgram
    { model = { first = Nothing, middle = Nothing, last = Nothing, focus = NoFocus }
    , update = update
    , view = view
    }
