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
  , padLeft
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
renderTitle :: Tree Note -> B.Widget n
renderTitle node =
  let note = root node
      n = case ((T.length . name) note) > 0 of
        True  -> B.txt (name note)
        False -> B.txt (T.pack " ")
      d = (renderDate . date) note
      s = (renderStatus . status) note
      p = (renderPriority . priority) note
      c = renderProgress node
  in  B.padRight (B.Pad 1) s
      B.<+> B.padRight B.Max n
      B.<+> p
      B.<+> (B.padLeft (B.Pad 1) d)
      B.<=> B.withAttr (B.attrName "progress") c

-- | Render a Tree's children, given rendering functions.
renderChildren
  :: (Tree a -> B.Widget n)
  -> (Tree a -> B.Widget n)
  -> Tree a
  -> B.Widget n
renderChildren _ _ (Leaf _) = B.emptyWidget
renderChildren f g (Branch _ lSibs focused rSibs) =
  let lSibs' = f <$> (reverse lSibs)
      focused' = g focused
      rSibs' = f <$> rSibs
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
renderPriority None = B.emptyWidget
renderPriority Low  = (B.withAttr (B.attrName "low") . B.str) "●"
renderPriority Mid  = (B.withAttr (B.attrName "mid") . B.str) "●"
renderPriority High = (B.withAttr (B.attrName "high") . B.str) "●"

-- | Render progress as `[x/y]`, given a tree.
renderProgress :: Tree Note -> B.Widget n
renderProgress (Leaf _) = B.emptyWidget
renderProgress (Branch _ lSibs focused rSibs) =
  let children = lSibs ++ [focused] ++ rSibs
      done = length (filter (status . root) children)
      strs = ["≡ [", show done, "/", show (length children), "]"]
  in  foldl (B.<+>) B.emptyWidget (B.str <$> strs)
