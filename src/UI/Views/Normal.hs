module UI.Views.Normal (render) where

import Brick.Types (Padding(Pad))
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
  ( (<=>)
  , hLimit
  , padLeftRight
  , padTop
  , vBox
  , visible
  )

import Core.Focused (Focused(..))
import Core.Tree (Tree(..))
import Core.Types (Note(..), Priority(..), Resource(Viewport), View)
import Core.Zipper (Ctx(..), Zipper(..))
import UI.Views.Util (color, frame, renderTree)

render :: View
render notes = [frame context]
  where
    (Zipper focused ctx) = extract notes
    narrow = hCenter . hLimit 76
    wNote attr = color attr . padTop (Pad 1) . renderTree
    wFocused = (visible . wNote "focused") focused
    context = case ctx of
      Root -> narrow wFocused
      (Path ctx' a lSibs rSibs) ->
        ( color "focused"
        . border
        . padLeftRight 1
        . renderTree
        ) (Branch a lSibs focused rSibs)
        <=> narrow children
        where
          children =
            vBox (wNote "unfocused" <$> reverse lSibs)
            <=> wFocused
            <=> vBox (wNote "unfocused" <$> rSibs)
