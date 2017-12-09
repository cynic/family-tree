import Html exposing (div, span, Html, input, text, beginnerProgram, node)
import Html.Attributes exposing (class, classList, title, attribute, size, defaultValue)
import Html.Events exposing (onInput, onClick)
import List exposing (indexedMap, map, concat, any, all, filter)
import Tuple exposing (first, second)
import Debug exposing (log, crash)

-- import Html.Events exposing (onMouseOver, onMouseOut, onClick, onBlur)
-- import Maybe exposing (withDefault)

-- MODEL

type alias FullName =
  { first : String
  , middle : List String
  , last : String
  }

type EditState =
  Fixed
  | Editing String -- stores the original value
  | Creating

type alias NewName =
  { first : (EditState, String)
  , middle : List (EditState, String)
  , last : (EditState, String)
  }

type alias EditableName =
  { original : FullName
  , proposed : NewName
  }

type Name =
  Full FullName
  | Editable EditableName
  | CreateNew NewName

type WhichName =
  First
  | Last
  | Middle Int

type alias Model =
  { data : Name
  , hasEditFocus : List WhichName -- a stack
  }

-- UPDATE

type Msg =
  EditFullNameClick
  | EditName WhichName
  | MoveRight Int
  | MoveLeft Int
  | TrashMiddle Int
  | AppendMiddle
  | GlobalConfirm
  | GlobalUndo
  | SubConfirm WhichName
  | SubUndo WhichName
  | NameContentChange (WhichName, String)

replaceAt : Int -> a -> List a -> List a
replaceAt n replacement list =
  indexedMap (\idx -> \x -> if idx == n then replacement else x) list

updateAt : Int -> (a -> a) -> List a -> List a
updateAt n update list =
  indexedMap (\idx -> \x -> if idx == n then update x else x) list

nth : Int -> List a -> a
nth n l =
  case l of
    [] -> crash "nth called for an invalid index"
    x::rest -> if n == 0 then x else nth (n-1) rest

update : Msg -> Model -> Model
update msg model =
  let
    editToFull {first, middle, last} =
      {first=second first, last=second last, middle=map (\(_,v) -> v) middle}
    fullToNew {first, middle, last} =
      let
        toNew x = (Fixed, x)
      in {first=toNew first, middle=map toNew middle, last=toNew last}
    fullToEditable full = { original=full, proposed=fullToNew full }
  in
    case msg of
      GlobalConfirm ->
        case model.data of
          Full _ -> crash "GlobalConfirm, but state shouldn't allow it!"
          Editable x -> {data=Full (editToFull x.proposed), hasEditFocus=[]}
          CreateNew x -> {model | data=Full (editToFull x)}
      GlobalUndo ->
        case model.data of
          Full _ -> crash "GlobalUndo, but state shouldn't allow it!"
          Editable x -> {model | data=Full x.original}
          -- TODO: should I allow this?  But then the model itself is invalidated... hmm
          CreateNew _ -> crash "Can't GlobalUndo a newly-created name!"
      EditFullNameClick ->
        case model.data of
          Full v -> {data=Editable (fullToEditable v), hasEditFocus=[First]}
          Editable _ -> crash "Already editable, how could this message be received?"
          CreateNew _ -> crash "Should be impossible to create-new via edit-clicking"
      SubConfirm which ->
        let
          update (_,s) = (Fixed, s)
          subConfirm value =
            case which of
              First -> {value | first=(Fixed, second value.first)}
              Last -> {value | last=(Fixed, second value.last)}
              Middle n -> {value | middle=updateAt n update value.middle}
          newFocus = filter (\x -> x /= which) model.hasEditFocus
        in
          case model.data of
            Full _ -> crash "SubConfirm, but state shouldn't allow it"
            Editable x -> {data=Editable {x | proposed=subConfirm x.proposed}, hasEditFocus=newFocus}
            CreateNew x -> {data=CreateNew (subConfirm x), hasEditFocus=newFocus}
      SubUndo which ->
        let
          origValue (x,_) =
            case x of
              Editing v -> (Fixed, v)
              _ -> crash "Original value here doesn't exist!  Why?"
          subUndo proposed =
            case which of
              First -> {proposed | first=origValue proposed.first}
              Last -> {proposed | last=origValue proposed.last}
              Middle n -> {proposed | middle=updateAt n origValue proposed.middle}
          newFocus = filter (\x -> x /= which) model.hasEditFocus
        in
          case model.data of
            Full _ -> crash "SubUndo, but state shouldn't allow it"
            Editable x -> {data=Editable {x | proposed=subUndo x.proposed}, hasEditFocus=newFocus}
            CreateNew x -> crash "Shouldn't be able to SubUndo from a CreateNew state"
      EditName which ->
        let
          update (_,s) = (Editing s, s)
          subConfirm value =
            let
              {first, last} = value
              ((_, firstV), (_, lastV)) = (first, last)
            in
              case which of
                First -> {value | first=(Editing firstV, firstV)}
                Last -> {value | last=(Editing lastV, lastV)}
                Middle n -> {value | middle=updateAt n update value.middle}
          newFocus = which::model.hasEditFocus
        in
          case model.data of
            Full _ -> crash "EditName, but state shouldn't allow it"
            Editable x -> {data=Editable {x | proposed=subConfirm x.proposed}, hasEditFocus=newFocus}
            CreateNew x -> {model | data=Editable {original=editToFull x, proposed=subConfirm x}, hasEditFocus=newFocus}
      NameContentChange (which, s) ->
        let
          update (editState,_) = (editState, s)
          alter value =
            case which of
              First -> {value | first=(first value.first, s)}
              Last -> {value | last=(first value.last, s)}
              Middle n -> {value | middle=updateAt n update value.middle}
        in
          case model.data of
            Full _ -> crash "NameContentChange, but state shouldn't allow it"
            Editable x -> {model | data=Editable {x | proposed=alter x.proposed}}
            CreateNew x -> {model | data=CreateNew (alter x)}
      _ -> model

noOp : Html Msg
noOp = node "script" [] []

view : Model -> Html Msg
view {data, hasEditFocus} =
  let
    firstOrLast which (editState, s) subConfirmContent =
      let
        sizeAttribute = size 12
        valueAttribute = defaultValue s
        inputFunc = onInput (\s -> NameContentChange (which, s))
        attributeList =
          sizeAttribute :: inputFunc :: valueAttribute ::
            case hasEditFocus of
              [] -> []
              x::_ -> if which == x then [attribute "data-autofocus" ""] else []
        editHtml =
          [ input attributeList []
          , div [class "tools"]
            [ div [class "sub-confirm-edit"] (subConfirmContent which) ]
          ]
      in
       case editState of
        Fixed ->
          let
            t = if which == Last then "Change surname" else "Change name"
          in
            [ text s
            , div [class "tools"]
              [ span [class "fa fa-edit", title t, onClick (EditName which)] [] ]
            ]
        Editing _ -> editHtml
        Creating -> editHtml
    editHtml (first, middle, last) globalConfirmContent subConfirmContent =
      let
        confirmHtml =
          case (first, last) of
            ((Fixed, _), (Fixed, _)) ->
              if all (\x -> Tuple.first x == Fixed) middle then
                div [class "confirm-edit"] globalConfirmContent
              else noOp -- no confirmation when sub-changes still exist
            _ -> noOp
      in
        div [class "name editing"]
          [ div [class "edit-items"]
            [ div [class "edit-item first"]
              (firstOrLast First first subConfirmContent)
            , div [class "edit-item"]
              [ span [class "fa fa-plus-circle fa-2x", title "Add new middle name"] [] ]
            , div [class "edit-item last"]
              (firstOrLast Last last subConfirmContent)
            ]
          , confirmHtml
          ]
    ifNotEmpty (f, m, l) which html =
      let
        s =
          case which of
            First -> second f
            Last -> second l
            Middle n -> second (nth n m)
      in
        if String.isEmpty <| String.trim s then noOp
        else html
  in
    case data of
      CreateNew {first, middle, last} ->
        let
          globalConfirmHtml =
            [ span [class "fa fa-check fa-2x", title "Confirm changes", onClick GlobalConfirm] [] ]
          subConfirmContent which =
            [ ifNotEmpty (first,middle,last) which
                (span [class "fa fa-check", title "confirm", onClick (SubConfirm which)] [])
            ]
        in
          editHtml (first, middle, last) globalConfirmHtml subConfirmContent
      Full {first, middle, last} ->
        let
          firstDiv = div [class "first"] [text first]
          middleDivs = map (\v -> div [class "middle"] [text v]) middle
          lastDiv = div [class "last"] [text last]
        in
          div [class "name full", title "Click to edit", onClick EditFullNameClick]
            (concat [[firstDiv], middleDivs, [lastDiv]])
      Editable {proposed} ->
        let
          globalConfirmHtml =
            [ span [class "fa fa-check fa-2x", title "Confirm changes", onClick GlobalConfirm] []
            , span [class "fa fa-times fa-2x", title "Ignore changes", onClick GlobalUndo] []
            ]
          subConfirmContent which =
            [ ifNotEmpty (first, middle, last) which
                (span [class "fa fa-check", title "Confirm", onClick (SubConfirm which)] [])
            , span [class "fa fa-times", title "Undo", onClick (SubUndo which)] []
            ]
          {first, middle, last} = proposed
        in
          editHtml (first, middle, last) globalConfirmHtml subConfirmContent

main : Program Basics.Never Model Msg
main =
  let
    newName = { first = (Creating, ""), middle = [], last = (Creating, "") }
    initial = { data = CreateNew newName, hasEditFocus = [First, Last] }
  in
    beginnerProgram
      { model = initial
      , update = update
      , view = view
      }
