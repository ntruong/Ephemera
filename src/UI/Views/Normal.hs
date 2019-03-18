module UI.Views.Normal (render) where

import Core.Types (View)
import UI.Views.Util (renderFocus, renderTree)

render :: View
render = renderFocus renderTree
