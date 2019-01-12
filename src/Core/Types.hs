module Core.Types
  ( State(..)
  , Mode(..)
  , Field(..)
  , Note(..)
  , Priority(..)
  , createState
  , empty
  ) where

import qualified Data.Text as T (Text, empty)
import qualified Brick.Widgets.Edit as B (Editor)
import Core.Zipper (Zipper)

-- | Represent the state of application. We use a zipper of notes, the current
-- mode of the application, an editor (for text interaction with the user), and
-- perhaps the previous state.
data State = State
  { zipper :: (Zipper Note)
  , mode   :: Mode
  , prev   :: (Maybe State)
  }

-- | The different modes the application is allowed to have.
data Mode = Normal
          | Pending Field (B.Editor T.Text ())
          | Help

-- | The different editable fields for a note (should be all of them).
data Field = Name
           | Desc
           | Date

-- | All the information a note should contain.
data Note = Note
  { name :: T.Text
  , desc :: T.Text
  , date :: Maybe T.Text -- TODO(ntruong): change this to Data.DateTime
  , status :: Bool
  , priority :: Priority
  }

-- | Priority for a note.
data Priority = None
              | Low
              | Mid
              | High
              deriving (Enum, Eq, Ord)

-- | Creates a starting state from a given zipper.
createState :: Zipper Note -> State
createState z = State z Normal Nothing

-- | An empty note.
empty :: Note
empty = Note T.empty T.empty Nothing False None
