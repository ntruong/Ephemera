module UI.Utils where

import qualified Data.Text as T (Text, cons, intercalate, pack)
import qualified Data.Text.Zipper as T (getLineLimit, lineLengths, moveCursor)
import qualified Brick.AttrMap as B (attrName)
import qualified Brick.Types as B (Padding(..), Widget)
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
import Core.Tree (Tree(..), root)
import Core.Types (Note(..), Priority(..))

-- | Move a text editor to the last possible character.
lastEdit :: B.Editor T.Text n -> B.Editor T.Text n
lastEdit ed = B.applyEdit f ed
  where
    -- f z = T.moveCursor (x, y) z
    f z = T.moveCursor (row, col) z
      where
        row = case T.getLineLimit z of
          Just n -> n - 1
          Nothing -> 0
        col = (maximum . T.lineLengths) z

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
