import FamilyTree.Update exposing (update, subscriptions)
import FamilyTree.View exposing (view)
import FamilyTree.Basic exposing (init, Message)
import DataTypes exposing (FamilyTree)
import Html exposing (..)

-- main : Program Never
main =
  Html.program { init=init, update=update, subscriptions=subscriptions, view=view }
