module Name exposing (Model, Msg(..), update, view, newName)
import PrimaryName
import Nickname
import Html exposing (div, Html, text)
import Html.Attributes exposing (class)
import Lib exposing (..)
import List
import Button exposing (Button(Big,Huge))

-- MODEL

type alias Name =
  { name : PrimaryName.Model
  , nicknames : List Nickname.Model
  }

type alias Model = Name

-- UPDATE

type Msg =
  PrimaryMsg PrimaryName.Msg
  | NickMsg (Int, Nickname.Msg)
  | AddNickname

update : Msg -> Model -> Model
update msg model =
  case msg of
    PrimaryMsg primaryMsg ->
      {model | name = PrimaryName.update primaryMsg model.name}
    NickMsg (idx, nickMsg) ->
      let
        doUpdate =
          indexedChoose (\i -> \nickname ->
            if i == idx then
              let
                value = Nickname.update nickMsg nickname
              in
                if Nickname.isDeleted value then Nothing
                else Just value
            else Just nickname
          )
      in
        {model | nicknames = doUpdate model.nicknames}
    AddNickname ->
      {model | nicknames = model.nicknames ++ [Nickname.newName]}

view : Model -> Html Msg
view model =
  div [class "all-names"]
    [ Html.map PrimaryMsg (PrimaryName.view model.name)
      , elideIf (List.isEmpty model.nicknames) (div [class "nicknames"])
        (
          ( model.nicknames
            |> List.indexedMap (\idx -> \nickname ->
              Html.map (\msg -> NickMsg (idx, msg)) (Nickname.view nickname)
            ) |> List.intersperse (text ", ")
            |> List.map Just
          ) ++ [Just <| widget (Button.Append "nickname") AddNickname]
        )
    ]

newName : Model
newName = {name = PrimaryName.newName, nicknames = []}
