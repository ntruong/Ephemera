module Main where

import qualified Brick.Main as B (App(..), defaultMain, showFirstCursor)
import qualified Brick.Themes as B (themeToAttrMap)
import Core.Types (State, createState)
import IO.Data (decode)
import UI.Handle (handle)
import UI.Render (render)
import UI.Theme (theme)

main :: IO State
main = do
  z <- decode
  let s = createState z
      app = B.App {
        B.appDraw = render
      , B.appChooseCursor = B.showFirstCursor
      , B.appHandleEvent = handle
      , B.appStartEvent = return
      , B.appAttrMap = (const . B.themeToAttrMap) theme
      }
  B.defaultMain app s
