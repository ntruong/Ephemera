module UI.Modes.List
  ( handle
  , render
  ) where

import qualified Control.Monad as M (filterM)
import qualified Control.Monad.IO.Class as M (liftIO)
import qualified Data.Either as E (either)
import qualified Data.Maybe as M (isJust, maybe)
import qualified Data.Text as T
  ( append
  , concat
  , cons
  , empty
  , intercalate
  , pack
  , reverse
  , take
  )
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Text.Regex.PCRE.ByteString as R
  ( compExtended
  , compile
  , execBlank
  , execute
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
import qualified Core.FList as F (FList(..), empty, focus, left, right)
import Core.Tree (root)
import Core.Types (Mode(..), Note(..), Resource(..), State(..))
import Core.Zipper (focus, list, top, up)
import UI.Utils

handle :: State -> B.BrickEvent Resource e -> B.EventM Resource (B.Next State)
handle s (B.VtyEvent e) = case e of
  V.EvKey key _ -> case key of
    V.KEsc -> B.continue (State z m p)
    V.KDown -> B.continue (State z (List (F.right flist) ed) p)
    V.KUp -> B.continue (State z (List (F.left flist) ed) p)
    V.KEnter -> B.continue (M.maybe s (\z' -> State z' m p) (F.focus flist))
    _ -> do
      let txtBS = T.encodeUtf8 . T.concat . B.getEditContents
          matchR eitherRegexp text =
            E.either
            -- If the regex doesn't compile, we fail with IO Bool.
            (return . const False)
            -- If the regex compiles, we have to execute the regex and check for
            -- a match.
            ( fmap (E.either (const False) M.isJust)
            . flip R.execute (T.encodeUtf8 text)
            )
            eitherRegexp
          possible = (list . top) z
          noteBS note = name note
            `T.append` T.pack " "
            `T.append` desc note
          makeFList xs = if null xs
                         then F.empty
                         else F.FList [] (Just (head xs)) (tail xs)
      ed' <- B.handleEditorEvent e ed
      eRegexp <- ( M.liftIO
                 . R.compile R.compExtended R.execBlank
                 . txtBS
                 ) ed'
      zips <- ( M.liftIO
              . M.filterM (matchR eRegexp . noteBS . root . focus)
              ) possible
      B.continue (State z (List (makeFList zips) ed') p)
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
