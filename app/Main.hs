module Main where

import qualified Brick.Main as B (App(..), defaultMain, showFirstCursor)
import qualified Brick.Themes as B (themeToAttrMap)
import qualified Brick.Widgets.Edit as B (editorText)
import qualified Data.Text as T (empty, pack)
import Core.Tree
import Core.Types
import Core.Zipper
import UI.Handle
import UI.Render
import UI.Theme

main :: IO State
main = B.defaultMain app s
  where
    app = B.App {
      B.appDraw = render
    , B.appChooseCursor = B.showFirstCursor
    , B.appHandleEvent = handle
    , B.appStartEvent = return
    , B.appAttrMap = (const . B.themeToAttrMap) theme
    }
    n = Note (T.pack "Ephemera") (T.pack "𝔈𝔭𝔥𝔢𝔪𝔢𝔯𝔞") Nothing False
    -- z = Zipper (Leaf n) (Path Root n [] [])
    z = Zipper (Leaf n) Root
    s = State z Normal Nothing
