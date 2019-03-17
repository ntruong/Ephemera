module UI.Views.Util
  ( color
  , frame
  , renderTree
  , renderZipper
  ) where

import qualified Data.Text as T (cons, null, pack)

import Brick.AttrMap (attrName)
import Brick.Widgets.Border (border)
import Brick.Types (Padding(..), ViewportType(..), Widget)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , emptyWidget
  , hLimit
  , padBottom
  , padLeft
  , padRight
  , padTop
  , str
  , txt
  , txtWrap
  , vBox
  , viewport
  , withAttr
  )

import Core.Focused (Focused(..))
import Core.Tree (Tree(..))
import Core.Types (Note(..), Priority(..), Resource(..))
import Core.Zipper (Zipper, root)

color :: String -> Widget Resource -> Widget Resource
color name = withAttr (attrName name)

frame :: Widget Resource -> Widget Resource
frame = hCenter . hLimit 80 . viewport Viewport Vertical

renderTree :: Tree Note -> Widget Resource
renderTree tree = note
  where
    (Note name' desc' date' status' priority') = extract tree
    wName     = txtWrap $ if T.null name' then T.pack " " else name'
    wDate     = if T.null date' then emptyWidget else txt $ T.cons '@' date'
    wDesc     = txtWrap desc'
    wStatus   = str $ if status' then "[✓]" else "[ ]"
    wPriority = case priority' of
      None -> emptyWidget
      Low  -> makeDot "low"
      Mid  -> makeDot "mid"
      High -> makeDot "high"
      where
        makeDot p = (color p . str) "●"
    wProgress = case tree of
      Leaf _ -> emptyWidget
      Branch _ x y z -> foldl (<+>) emptyWidget (str <$> msgs)
        where
          total = x ++ [y] ++ z
          done = length $ filter (status . extract) total
          msgs = ["≡ [", show done, "/", show (length total), "]"]
    note = padRight (Pad 1) wStatus
      <+> padRight Max wName
      <+> wPriority
      <+> padLeft (Pad 1) wDate
      <=> color "progress" wProgress
      <=> padTop (Pad 1) wDesc

renderZipper :: Zipper Note -> Widget Resource
renderZipper = padBottom (Pad 1) . renderTree . root
