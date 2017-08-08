module FamilyTree.Update exposing (update, subscriptions)
import FamilyTree.Basic exposing (..)
import Bootstrap.Modal exposing (..)
import Components.Name.Primary as PrimaryName

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Message
subscriptions model =
  closedModal (\_ -> ModalCancelled)

-- UPDATE

update : Message -> Model -> (Model, Cmd Message)
update action model =
  case action of
    PrimaryNameMessage msg ->
      let
        (updatedModel, widgetCmd) = PrimaryName.update msg model.primaryModel
      in
        ({model | primaryModel=updatedModel}, Cmd.map PrimaryNameMessage widgetCmd)
    _ ->
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
