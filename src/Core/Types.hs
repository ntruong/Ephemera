module Core.Types
  ( State
  , Notes
  , View
  , Handler(..)
  , Resource(..)
  , Note(..)
  , Priority(..)
  , editName
  , editDesc
  , editDate
  , toggleNote
  , decrNote
  , incrNote
  , blank
  ) where

import Data.Text (Text, empty)

import Brick.Types (BrickEvent, EventM, Next, Widget)
import Brick.Widgets.Edit (Editor)

import Core.Series (Series)
import Core.Tree (Tree)
import Core.Zipper (Zipper)

-- | Represent the state of the application as a mode with a series of zippers.
type Notes = Series (Zipper Note)
-- | Handler class; given a state and event, generate a new state and handler.
newtype Handler = Handler
  { handler
      :: Notes
      -> BrickEvent Resource ()
      -> EventM Resource (Next State)
  }
-- | View class; wrapper for Brick widgets.
type View = Notes -> [Widget Resource]
-- | Overall state of the application.
type State = (Notes, View, Handler)

-- | Resource names.
data Resource
  = Viewport
  | Editor
  deriving (Eq, Ord, Show)

-- | All the information a note should contain.
data Note = Note
  { name     :: Text
  , desc     :: Text
  , date     :: Text
  , status   :: Bool
  , priority :: Priority
  }

-- | Priority for a note.
data Priority
  = None
  | Low
  | Mid
  | High
  deriving (Bounded, Enum, Eq, Ord)

-- | Update a note's name.
editName :: Text -> Note -> Note
editName text (Note _ desc date status priority) =
  Note text desc date status priority

-- | Update a note's description.
editDesc :: Text -> Note -> Note
editDesc text (Note name _ date status priority) =
  Note name text date status priority

-- | Update a note's date.
editDate :: Text -> Note -> Note
editDate text (Note name desc _ status priority) =
  Note name desc text status priority

-- | Toggle note status.
toggleNote :: Note -> Note
toggleNote (Note name desc date status priority) =
  Note name desc date (not status) priority

-- | Decrease note priority.
decrNote :: Note -> Note
decrNote (Note name desc date status priority) =
  let priority' = if priority == minBound then minBound else pred priority
  in  Note name desc date status priority'

-- | Increase note priority.
incrNote :: Note -> Note
incrNote (Note name desc date status priority) =
  let priority' = if priority == maxBound then maxBound else succ priority
  in  Note name desc date status priority'

-- | An empty note.
blank :: Note
blank = Note empty empty empty False None
