module Components.Name exposing (..)

-- Model

type alias PrimaryName =
  { first : String
  , middle : List String
  , last : Maybe String
  }

-- The `Distinguisher` case is about distinguishing people who would otherwise
-- have the same name
type Name = Primary PrimaryName | Nickname String | Distinguisher String

-- Update

type Message =
  | FlipNameType

-- View
