module UI.Views.Preview (render) where

import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core (hLimit, padLeftRight)

import Core.Focused (Focused(..))
import Core.Types (View)
import Core.Zipper (root)
import UI.Views.Util (color, renderTree)

render :: View
render notes = [preview notes]
  where
    preview = hCenter
      . hLimit 80
      . color "focused"
      . border
      . padLeftRight 1
      . renderTree
      . root
      . extract
