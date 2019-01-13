module UI.Utils where

import qualified Data.Text as T (Text, cons, intercalate, length, pack)
import qualified Data.Text.Zipper as T (getText, moveCursor)
import qualified Brick.AttrMap as B (attrName)
import qualified Brick.Main as B (continue)
import qualified Brick.Types as B (EventM, Next, Padding(..), Widget)
import qualified Brick.Widgets.Core as B
  ( (<+>)
  , (<=>)
  , emptyWidget
  , padRight
  , str
  , txt
  , vBox
  , withAttr
  )
import qualified Brick.Widgets.Edit as B (Editor, applyEdit)
import Core.Tree (Tree(..), root, tLeft, tLeftmost, tRight, tRightmost)
import Core.Types (Mode(..), Note(..), Priority(..), State(..))
import Core.Zipper (Zipper, down, up, modifyM)

continueZipper :: State -> Zipper Note -> B.EventM () (B.Next State)
continueZipper s z = B.continue (State z (mode s) (prev s))

-- | Conduct a movement on zipper which may or may not succeed.
moveM :: (Zipper Note -> Maybe (Zipper Note))
      -> State
      -> B.EventM () (B.Next State)
moveM f s = case (f . zipper) s of
  Just z  -> continueZipper s z
  Nothing -> B.continue s

-- | Conduct a movement on the focus of a zipper which may or may not succeed.
moveFocusM :: (Tree Note -> Maybe (Tree Note))
           -> State
           -> B.EventM () (B.Next State)
moveFocusM f s = case modifyM f (zipper s) of
  Just z  -> continueZipper s z
  Nothing -> B.continue s

{-
    V.KChar 'h' -> case up z of
      Just z' -> B.continue (State z' Normal p)
      Nothing -> B.continue s -- TODO(ntruong): show error msg?
    -- Go "right" to next child at focus.
    V.KChar 'j' -> case modifyM tRight z of
      Just z' -> B.continue (State z' Normal p)
      Nothing -> B.continue s
    -- Go "left" to next child at focus.
    V.KChar 'k' -> case modifyM tLeft z of
      Just z' -> B.continue (State z' Normal p)
      Nothing -> B.continue s
    -- Go "down" the zipper.
    V.KChar 'l' -> case down z of
      Just z' -> B.continue (State z' Normal p)
      Nothing -> B.continue s -- TODO(ntruong): show error msg?
    -- Go to the leftmost child of the focus.
    V.KChar 'g' -> case modifyM tLeftmost z of
      Just z' -> B.continue (State z' Normal p)
      Nothing -> B.continue s
    -- Go to the rightmost child of the focus.
    V.KChar 'G' -> case modifyM tRightmost z of
      Just z' -> B.continue (State z' Normal p)
      Nothing -> B.continue s
    -- Edit the focus' name.
 -}

-- | Move a text editor to the last possible character.
lastEdit :: B.Editor T.Text n -> B.Editor T.Text n
lastEdit ed = B.applyEdit f ed
  where
    f z = T.moveCursor (row, col) z
      where
        text = T.getText z
        row  = max 0 ((length text) - 1)
        col  = (T.length . last) text

-- | Render a note as a title.
renderTitle :: Note -> B.Widget n
renderTitle note =
  let n = B.txt (name note)
      d = (renderDate . date) note
      s = (renderStatus . status) note
      p = (renderPriority . priority) note
  in  B.padRight (B.Pad 1) s
      B.<+> B.padRight (B.Pad 1) n
      B.<+> (B.padRight B.Max p)
      B.<+> d

-- | Render a Tree's children, given rendering functions.
renderChildren
  :: (a -> B.Widget n)
  -> (a -> B.Widget n)
  -> Tree a
  -> B.Widget n
renderChildren _ _ (Leaf _) = B.emptyWidget
renderChildren f g (Branch _ lSibs focused rSibs) =
  let lSibs' = (f . root) <$> (reverse lSibs)
      focused' = (g . root) focused
      rSibs' = (f . root) <$> rSibs
  in (B.vBox lSibs') B.<=> focused' B.<=> (B.vBox rSibs')

-- | Render a date.
renderDate :: Maybe (T.Text) -> B.Widget n
renderDate (Just x) = (B.txt . T.cons '@') x
renderDate Nothing  = B.emptyWidget

-- | Render a status as a checkbox.
renderStatus :: Bool -> B.Widget n
renderStatus True  = B.str "[✓]"
renderStatus False = B.str "[ ]"

-- | Render a priority as colored digraphs.
renderPriority :: Priority -> B.Widget n
renderPriority None = B.str " " -- B.emptyWidget shrinks padding for some reason
renderPriority Low  = (B.withAttr (B.attrName "low") . B.str) "●"
renderPriority Mid  = (B.withAttr (B.attrName "mid") . B.str) "●"
renderPriority High = (B.withAttr (B.attrName "high") . B.str) "●"
