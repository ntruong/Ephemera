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
import qualified Brick.Widgets.Center as B (center)
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
import Core.Types (Mode(..), Resource, State(..))

-- | Only respond to keyboard events, specifically, only respond to 'q' or 'Esc'
-- to quit the help menu.
handle :: State -> B.BrickEvent Resource e -> B.EventM Resource (B.Next State)
handle s (B.VtyEvent e) = case e of
  V.EvKey key _ -> case key of
    V.KEsc -> B.continue (State z m p)
    V.KChar 'q' -> B.continue (State z m p)
    _ -> B.continue s
    where
      z = zipper s
      p = prev s
      m = case mode <$> p of
        Just m' -> m'
        Nothing -> Normal []
  _ -> B.continue s
handle s _ = B.continue s

render :: State -> [B.Widget Resource]
render = const [helpMenu]

helpMenu :: B.Widget Resource
helpMenu =
  let doc = [ ("?"     , "Show this help menu" )
            , ("h"     , "Move left (higher)"  )
            , ("j"     , "Move down"           )
            , ("k"     , "Move up"             )
            , ("l"     , "Move right (deeper)" )
            , ("g"     , "Move to top"         )
            , ("G"     , "Move to bottom"      )
            , ("I"     , "Edit the name"       )
            , ("i"     , "Edit the description")
            , ("@"     , "Edit the date"       )
            , ("-"     , "Lower priority"      )
            , ("+"     , "Higher priority"     )
            , ("O"     , "Add task above"      )
            , ("o"     , "Add task below"      )
            , ("d"     , "Delete"              )
            , ("y"     , "Yank (additively)"   )
            , ("P"     , "Paste task above"    )
            , ("p"     , "Paste task below"    )
            , ("Space" , "Toggle status"       )
            , ("!"     , "Sort on date"        )
            , ("="     , "Sort on priority"    )
            , ("u"     , "Undo"                )
            , ("Esc"   , "Normal mode"         )
            , ("q"     , "Quit"                )
            ]
      inputs = (B.str . fst) <$> doc
      actions = (B.str . snd) <$> doc
      label = B.withAttr (B.attrName "special") (B.str "Controls")
      menu =
        ( B.padRight B.Max
        . B.withAttr (B.attrName "special")
        . B.hLimit 8 . B.vBox
        ) inputs
        B.<+> (B.vBox actions)
  in  ( B.center
      . B.hLimit 80
      . B.borderWithLabel label
      . B.padLeftRight 1
      ) menu
