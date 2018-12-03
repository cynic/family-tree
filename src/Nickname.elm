module Nickname exposing
    ( Model
    , Msg
    , isDeleted
    , newName
    , update
    , view
    )

import Button exposing (Button(..))
import Html exposing (Html, div, input, node, span, text)
import Html.Attributes exposing (attribute, class, classList, size, title)
import Html.Events exposing (onClick)
import Lib exposing (noOp, optionally, someHtml, widget)
import NameBlock as Block exposing (NameKind(..), NameOps(..))



-- MODEL


type alias Model =
    Block.Model



-- UPDATE


type alias Msg =
    Block.Msg


isDeleted : Model -> Bool
isDeleted model =
    model == Block.Gone


update : Msg -> Model -> Model
update msg model =
    Block.update msg model


view : Model -> Html Msg
view model =
    case model of
        Block.Fixed _ ->
            div [ class "nickname" ]
                [ div [ class "fixed", title "Click to edit", onClick Block.Edit ]
                    [ span [ class "formal-text" ] [ text "“" ]
                    , Block.view model
                    , span [ class "formal-text" ] [ text "”" ]
                    ]
                ]

        Block.Gone ->
            noOp

        Block.Editing _ ->
            div [ class "nickname" ]
                [ div [ class "editing" ]
                    [ Block.view model
                    , someHtml (div [ class "tools" ])
                        [ optionally (Block.canConfirm model) (\() -> widget Button.Confirm Block.Confirm)
                        , Just (widget Button.Cancel Block.Cancel)
                        , Just (widget (Button.Delete "nickname") Block.Delete)
                        ]
                    ]
                ]

        Block.Creating _ ->
            div [ class "nickname" ]
                [ div [ class "editing" ]
                    [ Block.view model
                    , someHtml (div [ class "tools" ])
                        [ optionally (Block.canConfirm model) (\() -> widget Button.Confirm Block.Confirm)
                        , Just (widget (Button.Delete "nickname") Block.Delete)
                        ]
                    ]
                ]


newName : Model
newName =
    Block.Creating ( { kind = Block.Nickname, allowed = [ CanDelete, CanCancel ] }, "" )



--main : Program Basics.Never Model Msg
--main =
--    beginnerProgram
--        { model = newName
--        , update = update
--        , view = view
--        }
