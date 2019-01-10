module UI.Modes.Pending
  ( handle
  , render
  ) where

import qualified Brick.Main as B (continue, halt)
import qualified Brick.Types as B (BrickEvent(..), EventM, Next, Widget)
import qualified Brick.Widgets.Edit as B
  ( getEditContents
  , editorText
  , handleEditorEvent
  )
import qualified Data.Text as T (intercalate, pack)
import qualified Graphics.Vty.Input.Events as V (Event(..), Key(..))
import Core.Tree (root)
import Core.Types (Field(..), Mode(..), Note(..), State(..))
import Core.Zipper (focus, modifyA)
import qualified UI.Modes.Normal as N (render)

handle :: State -> B.BrickEvent () e -> B.EventM () (B.Next State)
handle s@(State z (Pending fld ed) prev) (B.VtyEvent e) = case e of
  V.EvKey key _ -> case key of
    -- | Finish editing, return to normal mode.
    V.KEsc -> B.continue (State z Normal prev)
    _ -> do
      ed' <- B.handleEditorEvent e ed
      let txt = (T.intercalate (T.pack "\n") . B.getEditContents) ed'
          (Note nm de dt st) = (root . focus) z
          note = case fld of
            Name -> Note txt de dt st
            Desc -> Note nm txt dt st
            Date -> Note nm de (Just txt) st
          z' = modifyA (const note) z
      B.continue (State z' (Pending fld ed') prev)
  _ -> B.continue s
handle s _ = B.continue s

render :: State -> [B.Widget ()]
render = N.render
