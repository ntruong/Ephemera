module UI.Views.Edit (render) where

import qualified Data.Text as T (cons, intercalate, null, pack)

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , emptyWidget
  , padLeft
  , padRight
  , padTop
  , str
  , txt
  , txtWrap
  )
import Brick.Widgets.Edit (renderEditor)

import Core.Focused (Focused(..))
import Core.Types (EditNote(..), Priority(..), View)
import UI.Views.Util (color, renderFocus)

render :: EditNote -> View
render (EditNote name desc date st pr) = renderFocus (const note)
  where
    drawEditor = renderEditor (txt . T.intercalate (T.pack "\n")) True
    fName text
      | T.null text = T.pack " "
      | otherwise   = text
    wName     = either (txtWrap . fName) drawEditor name
    wDesc     = either txtWrap drawEditor desc
    fDate text
      | T.null text = text
      | otherwise   = T.cons '@' text
    wDate     = either (txt . fDate) drawEditor date
    wStatus   = str $ if st then "[✓]" else "[ ]"
    wPriority = case pr of
      None -> emptyWidget
      Low  -> makeDot "low"
      Mid  -> makeDot "mid"
      High -> makeDot "high"
      where
        makeDot p = (color p . str) "●"
    note = padRight (Pad 1) wStatus
      <+> padRight Max wName
      <+> wPriority
      <+> padLeft (Pad 1) wDate
      <=> padTop (Pad 1) wDesc
