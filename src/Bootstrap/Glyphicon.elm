module Bootstrap.Glyphicon exposing (icon, iconText, Icon(..))
import Html exposing (..)
import Html.Attributes exposing (..)
-- NOTE: Must add a courtesy link to glyphicons.com if I use this!

type Icon =
  Plus | Minus | Edit | Search | User | OK | Cancel
  | Settings | Trash | Picture | PlusSign | MinusSign | OKSign | CancelSign
  | OKCircle | CancelCircle | ExclamationSign | Calendar

iconToString : Icon -> String
iconToString icon =
  case icon of
    Plus -> "plus"
    Minus -> "minus"
    Edit -> "pencil"
    Search -> "search"
    User -> "user"
    OK -> "ok"
    Cancel -> "remove"
    Settings -> "cog"
    Trash -> "trash"
    Picture -> "picture"
    PlusSign -> "plus-sign"
    MinusSign -> "minus-sign"
    OKSign -> "ok-sign"
    CancelSign -> "remove-sign"
    OKCircle -> "ok-circle"
    CancelCircle -> "remove-circle"
    ExclamationSign -> "exclamation-sign"
    Calendar -> "calendar"

icon : Icon -> String -> Html a
icon icon s =
  span [class ("glyphicon glyphicon-" ++ (iconToString icon)), attribute "aria-label" s] []

iconText : Icon -> String -> Html a
iconText icon txt =
  span []
    [ span [class ("glyphicon glyphicon-" ++ (iconToString icon)), attribute "aria-hidden" "true"] []
    , text (" " ++ txt)
    ]
