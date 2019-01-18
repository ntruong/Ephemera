module UI.Modes.Normal
  ( handle
  , render
  ) where

import qualified Control.Monad.IO.Class as M (liftIO)
import qualified Data.Maybe as M (maybe)
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
import qualified Brick.Widgets.Center as B (hCenter)
import qualified Brick.Widgets.Core as B
  ( (<+>)
  , (<=>)
  , hLimit
  , padBottom
  , txtWrap
  , vBox
  , withAttr
  )
import qualified Brick.Widgets.Edit as B (editorText)
import qualified Graphics.Vty.Input.Events as V (Event(..), Key(..))
import qualified Core.FList as F (empty)
import Core.Tree
import Core.Types
  ( Field(..)
  , Mode(..)
  , Note(..)
  , Priority(..)
  , Resource(..)
  , State(..)
  , empty
  )
import Core.Zipper
import IO.Data (encode)
import UI.Utils

handle :: State -> B.BrickEvent Resource e -> B.EventM Resource (B.Next State)
handle s (B.VtyEvent e) = case e of
  V.EvKey key _ -> case key of
    -- Show help menu.
    V.KChar '?' -> B.continue (State z Help p)
    -- Go "up" the zipper.
    V.KChar 'h' -> moveM up s
    -- Go "right" to next child at focus.
    V.KChar 'j' -> moveFocusM tRight s
    -- Go "left" to next child at focus.
    V.KChar 'k' -> moveFocusM tLeft s
    -- Go "down" the zipper.
    V.KChar 'l' -> moveM down s
    -- Go to the leftmost child of the focus.
    V.KChar 'g' -> moveFocusM tLeftmost s
    -- Go to the rightmost child of the focus.
    V.KChar 'G' -> moveFocusM tRightmost s
    -- Edit the focus' name.
    V.KChar 'I' ->
      let ed = lastEdit $ B.editorText Editor (Just 1) ((name . root . focus) z)
      in  B.continue (State z (Edit Name ed) (Just s))
    -- Edit the focus' description.
    V.KChar 'i' ->
      let ed = lastEdit $ B.editorText Editor (Just 10) ((desc . root . focus) z)
      in  B.continue (State z (Edit Desc ed) (Just s))
    -- Edit the focus' description.
    V.KChar '@' ->
      let ed = case (date . root . focus) z of
            Just dt -> lastEdit $ B.editorText Editor (Just 1) dt
            Nothing -> lastEdit $ B.editorText Editor (Just 1) T.empty
      in  B.continue (State z (Edit Date ed) (Just s))
    -- Decrement the focus' priority.
    V.KChar '-' ->
      let decr pr = case pr of
                      None -> None
                      _   -> pred pr
          f (Note nm de dt st pr) = Note nm de dt st (decr pr)
      in  B.continue (State (modifyA f z) m (Just s))
    -- Increment the focus' priority.
    V.KChar '+' ->
      let incr pr = case pr of
                      High -> High
                      _   -> succ pr
          f (Note nm de dt st pr) = Note nm de dt st (incr pr)
      in  B.continue (State (modifyA f z) m (Just s))
    -- Add empty note before the focused child.
    V.KChar 'O' ->
      let z' = modify (tInsLeft (Leaf empty)) z
      in  B.continue (State z' m (Just s))
    -- Add empty note after the focused child.
    V.KChar 'o' ->
      let z' = modify (tInsRight (Leaf empty)) z
      in  B.continue (State z' m (Just s))
    -- Delete the focused child.
    V.KChar 'd' -> case modifyM tDelete z of
      Just z' -> B.continue (State z' m (Just s))
      Nothing -> B.continue s
    -- Yank the focused child into a list to be pasted elsewhere.
    V.KChar 'y' ->
      let t = M.maybe (Leaf empty) focus (down z)
      in  case modifyM tDelete z of
            Just z' -> B.continue (State z' (Normal (t:yanked)) (Just s))
            Nothing -> B.continue s
    -- Paste task(s) above.
    V.KChar 'P' ->
      -- We compose functions that successively insert trees.
      let f = foldl (.) id (tInsLeft <$> reverse yanked)
          z' = modify f z
      in  B.continue (State z' (Normal []) (Just s))
    V.KChar 'p' ->
      -- We compose functions that successively insert trees.
      let f = foldl (.) id (tInsRight <$> yanked)
          z' = modify f z
      in  B.continue (State z' (Normal []) (Just s))
    -- Toggle the focus' status.
    V.KChar ' ' ->
      let f (Note nm de dt st pr) = Note nm de dt (not st) pr
      in  B.continue (State (modifyA f z) m (Just s))
    -- Sort children based on date.
    V.KChar '!' ->
      let z' = modify (tSortOn (date . root)) z
      in  B.continue (State z' m (Just s))
    -- Sort children based on priority.
    V.KChar '=' ->
      let z' = modify (tReverse . tSortOn (priority . root)) z
      in  B.continue (State z' m (Just s))
    -- Search using regex matching against title + desc.
    V.KChar '/' ->
      let ed = B.editorText Editor (Just 1) T.empty
      in  B.continue (State z (List F.empty ed) p)
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
      m = mode s
      (Normal yanked) = m
  _ -> B.continue s
handle s _ = B.continue s

render :: State -> [B.Widget Resource]
render s =
  let node = (focus . zipper) s
      note = root node
      desc' = (B.txtWrap . desc) note
      children =
        let title = B.padBottom (B.Pad 1) . renderTitle
            f = B.withAttr (B.attrName "unfocused") . title
            g = B.withAttr (B.attrName "focused") . title
        in  renderChildren f g node
      note' = B.vBox [ ( B.padBottom (B.Pad 1)
                       . B.withAttr (B.attrName "title")
                       . renderTitle
                       ) node
                     , B.padBottom (B.Pad 1) desc'
                     , children
                     ]
  in  [(B.hCenter . B.hLimit 80) note']
