module FamilyTree.Update exposing (update, subscriptions)
import FamilyTree.Basic exposing (..)
import BootstrapModal exposing (..)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Message
subscriptions model =
  closedModal (\_ -> ModalCancelled)

-- UPDATE

update : Message -> Model -> (Model, Cmd Message)
update action model =
  case (model.appState, action) of
    (Default, CreateButtonClick) -> ({model | appState=CreatedModal}, showModal "create-modal")
    (CreatedModal, ModalCancelled) -> ({model | appState=Default}, Cmd.none)
    (CreatedModal, ValidateInput) -> ({model | appState=AllowCreation}, Cmd.none)
    (AllowCreation, ModalCancelled) -> ({model | appState=Default}, Cmd.none)
    (AllowCreation, InvalidateInput) -> ({model | appState=CreatedModal}, Cmd.none)
    (AllowCreation, ProceedWithCreation v) ->
      let
        {tree} = model
        newTree = {tree | nodes=v::tree.nodes}
      in
        ({model | appState=Default, tree=newTree}, Cmd.none)
    _ -> (model, Cmd.none)

{-
  Default
    ::CreateButtonClick: CreateModal
  CreatedModal
    ::ModalCancelled: Default
    ::ValidateInput: AllowCreation
  AllowCreation
    ::ModalCancelled: Default
    ::InvalidateInput: CreatedModal
    ::ProceedWithCreation: [update, then] Default
-}
