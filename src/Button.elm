module Button exposing (..)

type Button =
  MoveLeft
  | MoveRight
  | Edit String
  | Delete String
  | Confirm
  | Cancel
  | Append String
  | Big Button
  | Huge Button
