module UI.Views.Util
  ( anchor
  , color
  , renderFocus
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
  , padLeftRight
  , padRight
  , padTop
  , str
  , txt
  , txtWrap
  , vBox
  , viewport
  , visible
  , withAttr
  )

import Core.Focused (Focused(..))
import Core.Tree (Tree(..))
import Core.Types (EditNote(..), Note(..), Priority(..), Resource(..), View)
import Core.Zipper (Ctx(..), Zipper(..), root)

color :: String -> Widget Resource -> Widget Resource
color name = withAttr (attrName name)

anchor :: Widget Resource -> Widget Resource -> Widget Resource
anchor a b = (hCenter . hLimit 80) (padBottom (Pad 1) a <=> view b)
  where
    view = padLeftRight 2 . viewport Viewport Vertical

renderFocus :: (Tree Note -> Widget Resource) -> View
renderFocus f notes = [context]
  where
    (Zipper focused ctx) = extract notes
    wNote attr = color attr . padBottom (Pad 1)
    wFocused = (visible . wNote "focused" . f) focused
    context = case ctx of
      Root -> (hCenter . hLimit 80 . padLeftRight 2) wFocused
      (Path ctx' a lSibs rSibs) -> anchor parent children
        where
          parent =
            ( color "focused"
            . border
            . padLeftRight 1
            . renderTree
            ) (Branch a lSibs focused rSibs)
          children =
            vBox (wNote "unfocused" . renderTree <$> reverse lSibs)
            <=> wFocused
            <=> vBox (wNote "unfocused" . renderTree <$> rSibs)

renderTree :: Tree Note -> Widget Resource
renderTree tree = note
  where
    (Note name' desc' date' status' priority') = extract tree
    wName     = txtWrap $ if T.null name' then T.pack " " else name'
    wDesc     = txtWrap desc'
    wDate     = if T.null date' then emptyWidget else txt $ T.cons '@' date'
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
