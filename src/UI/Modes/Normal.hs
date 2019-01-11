module UI.Modes.Normal
  ( handle
  , render
  ) where

import qualified Brick.AttrMap as B (attrName)
import qualified Brick.Main as B (continue, halt)
import qualified Brick.Types as B
  ( BrickEvent(..)
  , EventM
  , Next
  , Padding(..)
  , Widget
  )
import qualified Brick.Widgets.Core as B
  ( (<+>)
  , (<=>)
  , emptyWidget
  , hLimit
  , padBottom
  , padRight
  , str
  , txt
  , vBox
  , withAttr
  )
import qualified Brick.Widgets.Edit as B (editorText)
import qualified Data.Text as T (cons, empty)
import qualified Graphics.Vty.Input.Events as V (Event(..), Key(..))
import Core.Tree
import Core.Types (Field(..), Mode(..), Note(..), State(..), empty)
import Core.Zipper

handle :: State -> B.BrickEvent () e -> B.EventM () (B.Next State)
handle s@(State z _ prev) (B.VtyEvent e) = case e of
  V.EvKey key _ -> case key of
    -- Show help menu.
    V.KChar '?' -> B.continue (State z Help prev)
    -- Go "up" the zipper.
    V.KChar 'h' -> case up z of
      Just z' -> B.continue (State z' Normal prev)
      Nothing -> B.continue s -- TODO(ntruong): show error msg?
    -- Go "right" to next child at focus.
    V.KChar 'j' -> case modifyM tRight z of
      Just z' -> B.continue (State z' Normal prev)
      Nothing -> B.continue s
    -- Go "left" to next child at focus.
    V.KChar 'k' -> case modifyM tLeft z of
      Just z' -> B.continue (State z' Normal prev)
      Nothing -> B.continue s
    -- Go "down" the zipper.
    V.KChar 'l' -> case down z of
      Just z' -> B.continue (State z' Normal prev)
      Nothing -> B.continue s -- TODO(ntruong): show error msg?
    -- Edit the focus' name.
    V.KChar 'I' ->
      let ed = B.editorText () Nothing ((name . root . focus) z)
      in  B.continue (State z (Pending Name ed) (Just s))
    -- Edit the focus' description.
    V.KChar 'i' ->
      let ed = B.editorText () Nothing ((desc . root . focus) z)
      in  B.continue (State z (Pending Desc ed) (Just s))
    -- Edit the focus' description.
    V.KChar '@' ->
      let ed = case (date . root . focus) z of
            Just dt -> B.editorText () Nothing dt
            Nothing -> B.editorText () Nothing T.empty
      in  B.continue (State z (Pending Date ed) (Just s))
    -- Add empty note before the focused child.
    V.KChar 'O' ->
      let z' = modify (tInsLeft (Leaf empty)) z
      in  B.continue (State z' Normal (Just s))
    -- Add empty note after the focused child.
    V.KChar 'o' ->
      let z' = modify (tInsRight (Leaf empty)) z
      in  B.continue (State z' Normal (Just s))
    -- Toggle the focus' status.
    V.KChar ' ' ->
      B.continue (State (modify (fmap toggle) z) Normal (Just s))
    -- Undo the last modification.
    V.KChar 'u' -> case prev of
      Just s' -> B.continue s'
      Nothing -> B.continue s
    -- Quit.
    V.KChar 'q' -> B.halt s
    -- Base case.
    _ -> B.continue s
  _ -> B.continue s
  where
    toggle (Note nm de dt st) = Note nm de dt (not st)
handle s _ = B.continue s

render :: State -> [B.Widget ()]
render (State z _ _) = [B.hLimit 80 note']
  where
    node = focus z
    note = root node
    makeTitle :: Note -> B.Widget n
    makeTitle n = B.padRight (B.Pad 1) status'
      B.<+> (B.padRight B.Max name')
      B.<+> date'
      where
        name' = B.withAttr (B.attrName "title") $ B.txt (name n)
        date' = case date n of
          Just x -> B.txt (T.cons '@' x)
          Nothing -> B.emptyWidget
        status' = case status n of
          True -> B.str "[✓]"
          False -> B.str "[ ]"
    desc' = B.txt (desc note)
    children = case node of
      Leaf _ -> B.emptyWidget
      Branch _ lNodes fNode rNodes ->
        B.vBox (makeTitleRoot <$> (reverse lNodes))
        B.<=> (B.withAttr (B.attrName "title") . makeTitleRoot) fNode
        B.<=> B.vBox (makeTitleRoot <$> rNodes)
      where
        makeTitleRoot = makeTitle . root
    note' = B.vBox [ B.padBottom (B.Pad 1) $ makeTitle note
                   , B.padBottom (B.Pad 1) desc'
                   , children
                   ]
