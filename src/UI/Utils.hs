module UI.Utils where

import qualified Data.Text as T (Text, cons, intercalate, pack)
import qualified Brick.Types as B (Padding(..), Widget)
import qualified Brick.Widgets.Core as B
  ( (<+>)
  , (<=>)
  , emptyWidget
  , padRight
  , str
  , txt
  , vBox
  )
import Core.Tree (Tree(..), root)
import Core.Types (Note(..))

-- | Render a note as a title.
renderTitle :: Note -> B.Widget n
renderTitle note =
  let n = B.txt (name note)
      d = (renderDate . date) note
      s = (renderStatus . status) note
  in  B.padRight (B.Pad 1) s
      B.<+> (B.padRight B.Max n)
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
