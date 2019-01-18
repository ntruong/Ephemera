module UI.Modes.List
  ( handle
  , render
  ) where

import qualified Data.Maybe as M (fromMaybe, maybe)
import qualified Data.Text as T
  ( append
  , concat
  , cons
  , empty
  , intercalate
  , reverse
  , take
  )
import qualified Brick.AttrMap as B (attrName)
import qualified Brick.Main as B (continue)
import qualified Brick.Types as B
  ( BrickEvent(..)
  , EventM
  , Next
  , Padding(..)
  , ViewportType(..)
  , Widget
  )
import qualified Brick.Widgets.Center as B (hCenter)
import qualified Brick.Widgets.Core as B
  ( (<+>)
  , (<=>)
  , emptyWidget
  , hLimit
  , padRight
  , str
  , txt
  , vBox
  , viewport
  , visible
  , vLimit
  , withAttr
  )
import qualified Brick.Widgets.Edit as B
  ( getEditContents
  , handleEditorEvent
  , renderEditor
  )
import qualified Graphics.Vty.Input.Events as V (Event(..), Key(..))
import qualified Core.FList as F (FList(..), empty, focus, left, list, right)
import Core.Tree (root)
import Core.Types (Mode(..), Note(..), Resource(..), State(..))
import Core.Zipper (Ctx(..), Zipper(..), focus, list, top, up)
import qualified UI.Modes.Normal as N (render)
import UI.Utils

handle :: State -> B.BrickEvent Resource e -> B.EventM Resource (B.Next State)
handle s (B.VtyEvent e) = case e of
  V.EvKey key _ -> case key of
    V.KEsc -> B.continue (State z m p)
    V.KDown -> B.continue (State z (List (F.right flist) ed) p)
    V.KUp -> B.continue (State z (List (F.left flist) ed) p)
    V.KEnter -> B.continue (M.maybe s (\z' -> State z' m p) (F.focus flist))
    _ -> do
      ed' <- B.handleEditorEvent e ed
      let txt = (T.concat . B.getEditContents) ed'
          regex = undefined -- compile some regexp here
          -- match against regex, e.g. match "r/.../g" . ?getText . root . focus
          f = const True
          zips = (filter f . list . top) z
          flist' = if   null zips
                   then F.empty
                   else F.FList [] (Just (head zips)) (tail zips)
      B.continue (State z (List flist' ed') p)
    where
      z = zipper s
      p = prev s
      m = M.maybe (Normal []) mode p
      (List flist ed) = mode s
  _ -> B.continue s
handle s _ = B.continue s

render :: State -> [B.Widget Resource]
render s =
  let (List zips ed) = mode s
      (F.FList ls m rs) = zips
      root' = root . focus
      title = name . root'
      path z =
        let g = T.append . path
            f = M.maybe (T.append T.empty) g (up z)
        in  (T.reverse . T.take 20 . T.reverse . f . T.cons '/' . title) z
      descs = desc . root'
      makeRow z =
        let zp = ( B.padRight (B.Pad 2)
                 . B.withAttr (B.attrName "special")
                 . B.txt
                 . path
                 ) z
            zd = (B.vLimit 1 . B.txt . descs) z
        in  zp B.<+> zd
      results =
        let search = B.str "/"
              B.<+> B.renderEditor (B.txt . T.intercalate T.empty) True ed
            ls' = B.vBox (makeRow <$> reverse ls)
            m'  = M.maybe
              B.emptyWidget
              (B.withAttr (B.attrName "focused") . B.visible . makeRow) m
            rs' = B.vBox (makeRow <$> rs)
        in  B.viewport Viewport B.Vertical (B.vBox [ls', m', rs'])
            B.<=> B.withAttr (B.attrName "special") search
  in  [(B.hCenter . B.hLimit 80) results]
