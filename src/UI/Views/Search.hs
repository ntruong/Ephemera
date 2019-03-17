module UI.Views.Search (render) where

import Data.Maybe (maybe)
import Data.Text (Text, empty, intercalate)

import Brick.Types (Padding(Pad), Widget)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , emptyWidget
  , padBottom
  , padLeftRight
  , str
  , txt
  , visible
  , vLimit
  )
import Brick.Widgets.Edit (Editor, renderEditor)

import Core.Focused (Focused(..))
import Core.Series (Series(..))
import Core.Types (Note, Resource, View)
import Core.Zipper (Zipper, root)
import UI.Views.Util (color, frame, renderTree, renderZipper)

render :: Maybe (Series (Zipper Note)) -> Editor Text Resource -> View
render results editor _ = [frame search]
  where
    wEditor = str "/" <+> renderEditor (txt . intercalate empty) True editor
    wResults = maybe emptyWidget renderSeries results
    search =
      ( padBottom (Pad 1)
      . border
      . padLeftRight 1
      ) wEditor
      <=> wResults

croppedZipper :: Zipper Note -> Widget Resource
croppedZipper = vLimit 7 . renderZipper

renderSeries :: Series (Zipper Note) -> Widget Resource
renderSeries (Series focus prev next) = wPrev <=> wFocus <=> wNext
  where
    wFocus = (visible . color "focused" . croppedZipper) focus
    wPrev  = (color "unfocused" . renderPrev) prev
    wNext  = (color "unfocused" . renderNext) next

renderPrev :: Maybe (Series (Zipper Note)) -> Widget Resource
renderPrev Nothing = emptyWidget
renderPrev (Just (Series x prev _)) = renderPrev prev <=> croppedZipper x

renderNext :: Maybe (Series (Zipper Note)) -> Widget Resource
renderNext Nothing = emptyWidget
renderNext (Just (Series x _ next)) = croppedZipper x <=> renderNext next
