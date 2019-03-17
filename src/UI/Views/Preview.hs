module UI.Views.Preview (render) where

import Brick.Widgets.Border (border)
import Brick.Widgets.Core (padLeftRight)

import Core.Focused (Focused(..))
import Core.Types (View)
import Core.Zipper (root)
import UI.Views.Util (color, frame, renderTree)

render :: View
render notes = [(frame . preview) notes]
  where
    preview = color "focused"
      . border
      . padLeftRight 1
      . renderTree
      . root
      . extract
