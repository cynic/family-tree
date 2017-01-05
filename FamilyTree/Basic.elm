module FamilyTree.Basic exposing (..)
import DataTypes

type AppState =
  Default
  | CreatedModal
  | AllowCreation

type alias Model =
  { tree : DataTypes.FamilyTree
  , appState : AppState
  }

{-
`CreatePerson` message should result in a ModalConfig... ahh, heck, no.  Update func shouldn't be in the business of generating Content!!

Right.  In narrative form, then, what needs to be done?

Create New Person:
  A Create button should exist.
  When someone clicks on the Create button, a modal dialog should be created.
  That modal dialog should be shown.
  If someone cancels out of it, the modal dialog should be destroyed.
  If fields are incorrect, then creation should not be possible.
  If creation is requested, then
    the desired node should be created, and
    the modal dialog should be destroyed.

In terms of states...
  Default
  Default + CreateModal
  Default + CreateModal + ActiveModal
  Canceled: Default
  Incorrect: Default + CreateModal + ActiveModal
  Create: Default + AlterModel
  Default

  Default
    ::CreateButtonClick: CreateModal
  CreateModal
    ::ModalShown: ActivateModal
  ActivateModal
    ::ModalCancelled: Default
    ::ValidateInput: AllowCreation
  AllowCreation
    ::ModalCancelled: Default
    ::InvalidateInput: ActivateModal
    ::ProceedWithCreation: [update, then] Default

In terms of "things that happen in our app that our component responds to"...
  CreateButtonClick
  ModalShown
  ModalCancelled
  ValidInput
  InvalidInput
  ProceedWithCreation

Now, the MESSAGES are all about state transitions.  And the MODEL is all about
states themselves...

Edit Existing Person:
  An Edit button should exist.
  When someone clicks on the Edit button, a modal dialog should be created.
  That modal dialog should be shown.
  The modal dialog should be populated with the existing data.
  If someone cancels out of it, the modal dialog should be destroyed.
  If fields are incorrect, then creation should not be possible.
  If creation is requested, then
    the desired node should be replaced, and
    the modal dialog should be destroyed.
-}
type Message =
  CreateButtonClick
  | ModalShown
  | ModalCancelled
  | ValidateInput
  | InvalidateInput
  | ProceedWithCreation DataTypes.PersonRecord
  | NoOp

init : (Model, Cmd Message)
init =
  let initialTree = { edges=[],nodes=[] }
  in
    ( {tree=initialTree, appState=Default}, Cmd.none)
