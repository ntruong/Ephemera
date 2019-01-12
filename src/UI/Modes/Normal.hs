module UI.Modes.Normal
  ( handle
  , render
  ) where

import qualified Control.Monad.IO.Class as M (liftIO)
import qualified Data.Text as T (cons, empty)
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
  , hLimit
  , padBottom
  , txt
  , vBox
  , withAttr
  )
import qualified Brick.Widgets.Edit as B (editorText)
import qualified Graphics.Vty.Input.Events as V (Event(..), Key(..))
import Core.Tree
import Core.Types
  (Field(..)
  , Mode(..)
  , Note(..)
  , Priority(..)
  , State(..)
  , empty
  )
import Core.Zipper
import IO.Data (encode)
import UI.Utils

handle :: State -> B.BrickEvent () e -> B.EventM () (B.Next State)
handle s (B.VtyEvent e) = case e of
  V.EvKey key _ -> case key of
    -- Show help menu.
    V.KChar '?' -> B.continue (State z Help p)
    -- Go "up" the zipper.
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
    V.KChar 'I' ->
      let ed = lastEdit $ B.editorText () (Just 1) ((name . root . focus) z)
      in  B.continue (State z (Pending Name ed) (Just s))
    -- Edit the focus' description.
    V.KChar 'i' ->
      let ed = lastEdit $ B.editorText () (Just 10) ((desc . root . focus) z)
      in  B.continue (State z (Pending Desc ed) (Just s))
    -- Edit the focus' description.
    V.KChar '@' ->
      let ed = case (date . root . focus) z of
            Just dt -> lastEdit $ B.editorText () (Just 1) dt
            Nothing -> lastEdit $ B.editorText () (Just 1) T.empty
      in  B.continue (State z (Pending Date ed) (Just s))
    -- Decrement the focus' priority.
    V.KChar '-' ->
      let decr pr = case pr of
                      None -> None
                      _   -> pred pr
          f (Note nm de dt st pr) = Note nm de dt st (decr pr)
      in  B.continue (State (modifyA f z) Normal (Just s))
    -- Increment the focus' priority.
    V.KChar '+' ->
      let incr pr = case pr of
                      High -> High
                      _   -> succ pr
          f (Note nm de dt st pr) = Note nm de dt st (incr pr)
      in  B.continue (State (modifyA f z) Normal (Just s))
    -- Add empty note before the focused child.
    V.KChar 'O' ->
      let z' = modify (tInsLeft (Leaf empty)) z
      in  B.continue (State z' Normal (Just s))
    -- Add empty note after the focused child.
    V.KChar 'o' ->
      let z' = modify (tInsRight (Leaf empty)) z
      in  B.continue (State z' Normal (Just s))
    -- Delete the focused child.
    V.KChar 'd' -> case modifyM tDelete z of
      Just z' -> B.continue (State z' Normal (Just s))
      Nothing -> B.continue s
    -- Toggle the focus' status.
    V.KChar ' ' ->
      let f (Note nm de dt st pr) = Note nm de dt (not st) pr
      in  B.continue (State (modifyA f z) Normal (Just s))
    -- Undo the last modification.
    V.KChar 'u' -> case p of
      Just s' -> B.continue s'
      Nothing -> B.continue s
    -- Quit.
    V.KChar 'q' -> do
      M.liftIO (encode z)
      B.halt s
    -- Base case.
    _ -> B.continue s
    where
      z = zipper s
      p = prev s
  _ -> B.continue s
handle s _ = B.continue s

render :: State -> [B.Widget ()]
render s =
  let node = (focus . zipper) s
      note = root node
      desc' = B.txt (desc note)
      children =
        let f = (B.withAttr (B.attrName "focus")) . renderTitle
        in  renderChildren renderTitle f node
      note' = B.vBox [ ( B.padBottom (B.Pad 1)
                       . B.withAttr (B.attrName "title")
                       . renderTitle
                       ) note
                     , B.padBottom (B.Pad 1) desc'
                     , children
                     ]
  in  [B.hLimit 80 note']
