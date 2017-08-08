port module Bootstrap.Modal exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Button a =
  { text : String
  , class : String
  , message : a
  }

type alias ModalConfig a =
  { id : String
  , title : String
  , label : String
  , buttons : List (Button a)
  , content : Html a
  }

makeModal : ModalConfig a -> Html a
makeModal config =
  div [class "modal fade", id config.id, tabindex -1, attribute "role" "dialog", attribute "aria-labelledby" config.label]
    [ div [class "modal-dialog", attribute "role" "document"]
      [ div [class "modal-content"]
        [ div [class "modal-header"]
          [ button [type_ "button", class "close", attribute "data-dismiss" "modal", attribute "aria-label" "Close"]
            [ span [attribute "aria-hidden" "true"] [text "❎"] ]  -- ×, or ❌, or ❎ ?  Oh, the choices we make.
          , h4 [class "modal-title"] [text config.title]
          ]
        , div [class "modal-body"] [config.content]
        , div [class "modal-footer"] (List.map (\btn -> button [type_ "button", class btn.class, onClick btn.message] [text btn.text]) config.buttons)
        ]
      ]
    ]

-- for sending out to JS; input is the id of the modal
port showModal : String -> Cmd a

-- for listening to events from the modal JS
port closedModal : (String -> a) -> Sub a
port shownModal : (String -> a) -> Sub a
