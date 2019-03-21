module Core.Types
  ( State
  , Notes
  , View
  , Handler(..)
  , Resource(..)
  , Note(..)
  , EditNote(..)
  , Priority(..)
  , editName
  , editDesc
  , editDate
  , toggleNote
  , decrNote
  , incrNote
  , blank
  ) where

import qualified Data.Text as T (Text, empty, length)
import qualified Data.Text.Zipper as T (getText, moveCursor)

import Brick.Types (BrickEvent, EventM, Next, Widget)
import Brick.Widgets.Edit (Editor, applyEdit, editorText)

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
  { name     :: T.Text
  , desc     :: T.Text
  , date     :: T.Text
  , status   :: Bool
  , priority :: Priority
  }

-- | Edit mode note; the various text fields may be editors.
data EditNote = EditNote
  { eName     :: Either T.Text (Editor T.Text Resource)
  , eDesc     :: Either T.Text (Editor T.Text Resource)
  , eDate     :: Either T.Text (Editor T.Text Resource)
  , eStatus   :: Bool
  , ePriority :: Priority
  }

-- | Priority for a note.
data Priority
  = None
  | Low
  | Mid
  | High
  deriving (Bounded, Enum, Eq, Ord)

-- | Create an Editor from a Text with a given height and cursor on the last
-- character.
editor :: T.Text -> Int -> Editor T.Text Resource
editor text n = applyEdit lastChar $ editorText Editor (Just n) text
  where
    lastChar textZipper = T.moveCursor (row, col) textZipper
      where
        zipperText = T.getText textZipper
        row = max 0 (length zipperText - 1)
        col = (T.length . last) zipperText

-- | Turn a note into an editable note (with the name field).
editName :: Note -> EditNote
editName (Note name desc date status priority) =
  EditNote
  (Right $ editor name 1)
  (Left desc)
  (Left date)
  status
  priority

-- | Turn a note into an editable note (with the desc field).
editDesc :: Note -> EditNote
editDesc (Note name desc date status priority) =
  EditNote
  (Left name)
  (Right $ editor desc 10)
  (Left date)
  status
  priority

-- | Turn a note into an editable note (with the date field).
editDate :: Note -> EditNote
editDate (Note name desc date status priority) =
  EditNote
  (Left name)
  (Left desc)
  (Right $ editor date 1)
  status
  priority

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
blank = Note T.empty T.empty T.empty False None
