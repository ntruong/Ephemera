module UI.Modes.Edit
  ( handle
  , render
  ) where

import qualified Data.Text as T (Text, intercalate, null, pack)
import qualified Brick.AttrMap as B (attrName)
import qualified Brick.Main as B (continue, halt)
import qualified Brick.Types as B
  (BrickEvent(..)
  , EventM
  , Next
  , Padding(..)
  , Widget
  )
import qualified Brick.Widgets.Center as B (hCenter)
import qualified Brick.Widgets.Core as B
  ( (<+>)
  , (<=>)
  , emptyWidget
  , hLimit
  , padBottom
  , padLeft
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
    V.KEsc -> B.continue (State z m p)
    _ -> do
      ed' <- B.handleEditorEvent e ed
      let txt = (joinText . B.getEditContents) ed'
          (Note nm de dt st pr) = (root . focus) z
          note = case fld of
            Name -> Note txt de dt st pr
            Desc -> Note nm txt dt st pr
            Date -> case T.null txt of
              True  -> Note nm de Nothing st pr
              False -> Note nm de (Just txt) st pr
          z' = modifyA (const note) z
      B.continue (State z' (Edit fld ed') p)
    where
      z = zipper s
      p = prev s
      (Edit fld ed) = mode s
      m = case mode <$> p of
        Just m' -> m'
        Nothing -> Normal []
  _ -> B.continue s
handle s _ = B.continue s

render :: State -> [B.Widget ()]
-- render = N.render
render s =
  let (Edit fld ed) = mode s
      node = (focus . zipper) s
      note = root node
      txt = B.renderEditor (B.txt . joinText) True ed
      (name', desc', date') = case fld of
        Name -> (txt, (B.txt . desc) note, (renderDate . date) note)
        Desc -> ((B.txt . name) note, txt, (renderDate . date) note)
        Date -> ((B.txt . name) note, (B.txt . desc) note, txt)
      status' = (renderStatus . status) note
      priority' = (renderPriority . priority) note
      progress' = renderProgress node
      children =
        let f = B.padBottom (B.Pad 1)
              . (B.withAttr (B.attrName "focus"))
              . renderTitle
        in  renderChildren (B.padBottom (B.Pad 1) . renderTitle) f node
      title = B.padRight (B.Pad 1) status'
              B.<+> B.padRight B.Max name'
              B.<+> priority'
              B.<+> (B.padLeft (B.Pad 1) date')
              B.<=> B.withAttr (B.attrName "progress") progress'
      note' = B.vBox [ ( B.padBottom (B.Pad 1)
                       . B.withAttr (B.attrName "title")
                       ) title
                     , B.padBottom (B.Pad 1) desc'
                     , children
                     ]
  in  [(B.hCenter . B.hLimit 80) note']

-- | Get an array of Text as separate lines.
joinText :: [T.Text] -> T.Text
joinText = T.intercalate (T.pack "\n")
