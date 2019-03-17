module Main where

import Brick.Main (App(..), defaultMain, showFirstCursor)
import Brick.Themes (themeToAttrMap)
import Core.Series (new)
import Core.Types (State)
import IO.Data (decode)
import UI.App (render, handle)
import UI.Handlers (normal)
import UI.Theme (theme)
import qualified UI.Views.Normal as Normal (render)

main :: IO State
main = do
  z <- decode
  let s = (new z, Normal.render, normal [])
      app = App {
        appDraw = render
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handle
      , appStartEvent = return
      , appAttrMap = (const . themeToAttrMap) theme
      }
  defaultMain app s
