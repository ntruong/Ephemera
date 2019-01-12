module UI.Modes.Pending
  ( handle
  , render
  ) where

import qualified Data.Text as T (Text, intercalate, pack)
import qualified Brick.AttrMap as B (attrName)
import qualified Brick.Main as B (continue, halt)
import qualified Brick.Types as B
  (BrickEvent(..)
  , EventM
  , Next
  , Padding(..)
  , Widget
  )
import qualified Brick.Widgets.Core as B
  ( (<+>)
  , emptyWidget
  , hLimit
  , padBottom
  , padRight
  , txt
  , vBox
  , withAttr
  )
import qualified Brick.Widgets.Edit as B
  ( getEditContents
  , editorText
  , handleEditorEvent
  , renderEditor
  )
import qualified Graphics.Vty.Input.Events as V (Event(..), Key(..))
import Core.Tree (root)
import Core.Types (Field(..), Mode(..), Note(..), State(..))
import Core.Zipper (focus, modifyA)
import UI.Utils

handle :: State -> B.BrickEvent () e -> B.EventM () (B.Next State)
handle s (B.VtyEvent e) = case e of
  V.EvKey key _ -> case key of
    -- | Finish editing, return to normal mode.
    V.KEsc -> B.continue (State z Normal p)
    _ -> do
      ed' <- B.handleEditorEvent e ed
      let txt = (joinText . B.getEditContents) ed'
          (Note nm de dt st) = (root . focus) z
          note = case fld of
            Name -> Note txt de dt st
            Desc -> Note nm txt dt st
            Date -> Note nm de (Just txt) st
          z' = modifyA (const note) z
      B.continue (State z' (Pending fld ed') p)
    where
      z = zipper s
      p = prev s
      (Pending fld ed) = mode s
  _ -> B.continue s
handle s _ = B.continue s

render :: State -> [B.Widget ()]
-- render = N.render
render s =
  let (Pending fld ed) = mode s
      node = (focus . zipper) s
      note = root node
      txt = B.renderEditor (B.txt . joinText) True ed
      (name', desc', date') = case fld of
        Name -> (txt, (B.txt . desc) note, (renderDate . date) note)
        Desc -> ((B.txt . name) note, txt, (renderDate . date) note)
        Date -> ((B.txt . name) note, (B.txt . desc) note, txt)
      status' = (renderStatus . status) note
      children =
        let f = (B.withAttr (B.attrName "title")) . renderTitle
        in  renderChildren renderTitle f node
      title = B.padRight (B.Pad 1) status'
              B.<+> (B.padRight B.Max name')
              B.<+> date'
      note' = B.vBox [ ( B.padBottom (B.Pad 1)
                       . B.withAttr (B.attrName "title")
                       ) title
                     , B.padBottom (B.Pad 1) desc'
                     , children
                     ]
  in  [B.hLimit 80 note']

-- | Get an array of Text as separate lines.
joinText :: [T.Text] -> T.Text
joinText = T.intercalate (T.pack "\n")
