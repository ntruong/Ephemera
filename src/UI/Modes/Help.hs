module UI.Modes.Help
  ( handle
  , render
  ) where

import qualified Brick.AttrMap as B (attrName)
import qualified Brick.Main as B (continue)
import qualified Brick.Types as B
  ( BrickEvent(..)
  , EventM
  , Next
  , Padding(Max)
  , Widget
  )
import qualified Brick.Widgets.Center as B (hCenter, vCenter)
import qualified Brick.Widgets.Border as B (borderWithLabel)
import qualified Brick.Widgets.Core as B
  ( (<+>)
  , fill
  , hLimit
  , padLeftRight
  , padRight
  , str
  , vBox
  , vLimit
  , withAttr
  )
import qualified Graphics.Vty.Input.Events as V (Event(..), Key(..))
import Core.Types (Mode(..), State(..))

-- | Only respond to keyboard events, specifically, only respond to 'q' or 'Esc'
-- to quit the help menu.
handle :: State -> B.BrickEvent () e -> B.EventM () (B.Next State)
handle s@(State z _ prev) (B.VtyEvent e) = case e of
  V.EvKey key _ -> case key of
    V.KEsc -> B.continue (State z Normal prev)
    V.KChar 'q' -> B.continue (State z Normal prev)
    _ -> B.continue s
  _ -> B.continue s
handle s _ = B.continue s

render :: State -> [B.Widget ()]
render = const [helpMenu]

helpMenu :: B.Widget ()
helpMenu =
  let doc = [ ("?"     , "Show this help menu" )
            , ("h"     , "Move left (higher)"  )
            , ("j"     , "Move down"           )
            , ("k"     , "Move up"             )
            , ("l"     , "Move right (deeper)" )
            , ("I"     , "Edit the name"       )
            , ("i"     , "Edit the description")
            , ("@"     , "Edit the date"       )
            , ("O"     , "Add task above"      )
            , ("o"     , "Add task below"      )
            , ("d"     , "Delete"              )
            , ("Space" , "Toggle status"       )
            , ("u"     , "Undo"                )
            , ("Esc"   , "Normal mode"         )
            , ("q"     , "Quit"                )
            ]
      inputs = (B.str . fst) <$> doc
      actions = (B.str . snd) <$> doc
      label = B.withAttr (B.attrName "title") (B.str "Controls")
      menu =
        ( B.padRight B.Max
        . B.withAttr (B.attrName "special")
        . B.hLimit 8 . B.vBox
        ) inputs
        B.<+> (B.vBox actions)
  in  ( B.hCenter
      . B.hLimit 80
      . B.borderWithLabel label
      . B.padLeftRight 1
      ) menu
