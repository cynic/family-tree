module DataTypes exposing (..)

type alias PrimaryName =
  { first : String
  , middle : List String
  , last : Maybe String
  }

-- The `Distinguisher` case is about distinguishing people who would otherwise
-- have the same name
type Name =
  Primary PrimaryName
  --| Nickname String
  --| Distinguisher String

-- type alias Y = { year : Int }
-- type alias YM = { year : Int, month : Int }
type alias YMD = { year : Int, month : Int, day : Int }

-- The second one is the variance, in the smallest units of the first one.
-- So, e.g., years +- some number of years.  Or years & months, +- some number
-- of months.
-- type DateCirca = CircaY (Y,Int) | CircaYM (YM,Int) | CircaYMD (YMD,Int)

-- BeforeEvent etc should let me create a partial ordering.
type Date =
  Exact YMD
  --| Circa DateCirca
  --| Before Date
  --| After Date
  --| Between (Date, Date)
  --| BeforeEvent Event
  --| AfterEvent Event

-- Description * When
-- type alias Event = { description : String, when : Date }

-- Occupation(job, since)
-- Media(mimetype, source)
-- type Attribute =
--   Email String | Occupation (String, Maybe Date) | Media (String, String) | Story String

type Gender = Male | Female

--type alias BirthRecord =
--  { date : Maybe Date
--  , place : Maybe String
--  }

type alias PersonRecord =
  { id : String -- is this a cryptographic hash??
--  , name : List (Name, Maybe Date)
  , name : Name
--  , birth : BirthRecord
--  , death : Maybe Date
  , alive : Bool
  , gender : Gender
--  , events : List Event
--  , attributes : List Attribute
}

type DownLink =
  Biological
  | Foster (Maybe Date)

type alias DownLinkRecord =
  { up : String
  , down : String
  , linkType : DownLink
  }

type SideLink = Spouse | Partner

-- We can tell, from the PersonRecords, whether
-- the relationship was ended by a death.
-- We can also tell, from the Date, whether it's an
-- ex-relationship.
type alias SideLinkRecord =
  { a : String
  , b : String
  , linkType : SideLink
  --, date : Date
  }

-- A family tree is a graph of nodes & edges.
-- The edges are either DownLinks or SideLinks, and the nodes are PersonRecords
type Edge = Down DownLinkRecord | Side SideLinkRecord

type alias FamilyTree =
  { edges : List Edge
  , nodes : List PersonRecord
  }
