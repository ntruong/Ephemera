module UI.Handlers (normal) where

import qualified Control.Arrow as A (right)
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Either (either)
import Data.List (uncons)
import Data.Maybe (fromMaybe, isJust, maybe)
import qualified Data.Text as T (Text, concat, empty, intercalate, length, pack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Zipper as T (getText, moveCursor)

import Brick.AttrMap (attrName)
import Brick.Main (continue, halt)
import Brick.Types (BrickEvent(..), Padding(Pad), Widget)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
  ( hLimit
  , padBottom
  , vBox
  , withAttr
  )
import Brick.Widgets.Edit
  ( Editor
  , applyEdit
  , editorText
  , getEditContents
  , handleEditorEvent
  )
import Graphics.Vty.Input.Events (Event(..), Key(..))
import Text.Regex.PCRE.ByteString
  ( compExtended
  , compile
  , execBlank
  , execute
  )

import Core.Focused (Focused(..))
import Core.Serial (Serial(..))
import Core.Series (Series, (>>.), backwards, forwards)
import Core.Tree (Tree(..))
import Core.Types
  ( Handler(..)
  , Note(..)
  , EditNote(..)
  , Notes
  , Resource(..)
  , View
  , toggleNote
  , decrNote
  , incrNote
  , blank
  )
import Core.Zipper
  ( Zipper(..)
  , Ctx(..)
  , root
  , up
  , down
  , left
  , leftmost
  , right
  , rightmost
  , delete
  , append
  , prepend
  , top
  , sortOn
  , list
  )
import IO.Data (encode)
import qualified UI.Views.Edit as Edit (render)
import qualified UI.Views.Help as Help (render)
import qualified UI.Views.Normal as Normal (render)
import qualified UI.Views.Preview as Preview (render)
import qualified UI.Views.Search as Search (render)

-- | Normal mode handler; maintain a list of yanked notes.
normal :: [Tree Note] -> Handler
normal yanked = Handler handler
  where
    normal' = normal yanked

    handler notes (VtyEvent (EvKey (KChar key) _)) = case key of
      '?' -> continue (notes, Help.render, help yanked)
      'q' -> do
        liftIO $ (encode . extract) notes
        halt (notes, Normal.render, normal')
      'h' -> move up
      'j' -> move right
      'k' -> move left
      'l' -> case extract notes of
        Zipper (Leaf _) _ -> continue (notes, Preview.render, preview yanked)
        _ -> move down
      'g' -> move leftmost
      'G' -> move rightmost
      'O' -> update (left  . prepend emptyNode)
      'o' -> update (right . append  emptyNode)
      'd' -> update delete
      'y' -> continue
        ( delete >>. notes
        , Normal.render
        , normal ((root . extract) notes : yanked)
        )
      'P' -> continue
        ( foldl (.) id (prepend <$> reverse yanked) >>. notes
        , Normal.render
        , normal []
        )
      'p' -> continue
        ( foldl (.) id (append <$> yanked) >>. notes
        , Normal.render
        , normal []
        )
      'u' -> continue (backwards notes, Normal.render, normal')
      'r' -> continue (forwards notes,  Normal.render, normal')
      'i' -> editor editName
      'a' -> editor editDesc
      '@' -> editor editDate
      ' ' -> update (toggleNote <.>)
      '-' -> update (decrNote <.>)
      '+' -> update (incrNote <.>)
      '!' -> update (sortOn (date . extract))
      '=' -> update (sortOn (negate . fromEnum . priority . extract))
      '/' -> continue
        ( notes
        , Search.render results ed
        , search yanked results ed
        )
      _   -> continue (notes, Normal.render, normal')
      where
        move f = continue (f <.> notes, Normal.render, normal')
        update f = continue (f >>. notes, Normal.render, normal')
        moveToEnd z = T.moveCursor (row, col) z
          where
            text = T.getText z
            row  = max 0 (length text - 1)
            col  = (T.length . last) text
        loadEditor field n =
          applyEdit moveToEnd $ editorText Editor (Just n) field
        (Note name' desc' date' st' pr') = (extract . extract) notes
        editName = EditNote
          (Right $ loadEditor name' 1)
          (Left desc')
          (Left date')
          st'
          pr'
        editDesc = EditNote
          (Left name')
          (Right $ loadEditor desc' 10)
          (Left date')
          st'
          pr'
        editDate = EditNote
          (Left name')
          (Left desc')
          (Right $ loadEditor date' 1)
          st'
          pr'
        editor field = continue (notes, Edit.render field, edit yanked field)
        results = uncurry serialize <$> (uncons . list . top . extract) notes
        ed = editorText Editor (Just 1) T.empty

    handler notes _ = continue (notes, Normal.render, normal')

-- | Preview mode handler; maintain a list of yanked notes and restrict actions
-- to only those associated with node creation.
preview :: [Tree Note] -> Handler
preview yanked = Handler handler
  where
    handler notes (VtyEvent vty@(EvKey (KChar key) _)) = case key of
      'h' -> continue (notes, Normal.render, normal yanked)
      'o' -> makeBranch
      'O' -> makeBranch
      'q' -> halt (notes, Normal.render, normal yanked)
      '/' -> continue
        ( notes
        , Search.render results ed
        , search yanked results ed
        )
      _   -> continue (notes, Preview.render, preview yanked)
      where
        (Zipper (Leaf a) ctx) = extract notes
        zip = down $ Zipper (Branch a [] emptyNode []) ctx
        makeBranch = continue
          ( const zip >>. notes
          , Normal.render
          , normal yanked
          )
        results = uncurry serialize <$> (uncons . list . top . extract) notes
        ed = editorText Editor (Just 1) T.empty

    handler notes _ = continue (notes, Preview.render, preview yanked)

-- | Edit mode handler; maintain a list of yanked notes and update according to
-- a provided update function (e.g. editName, etc).
edit
  :: [Tree Note]
  -> EditNote
  -> Handler

edit yanked eNote@(EditNote eName eDesc eDate eSt ePr) = Handler handler
  where
    handler notes (VtyEvent vty@(EvKey key _)) = case key of
      KEsc -> continue
        ( (const note <.>) >>. notes
        , Normal.render
        , normal yanked
        )
      _    -> do
        -- let handleEvent = A.right (handleEditorEvent vty)
        let handleEvent (Right ed) = Right <$> handleEditorEvent vty ed
            handleEvent field = pure field
        eName' <- handleEvent eName
        eDesc' <- handleEvent eDesc
        eDate' <- handleEvent eDate
        let eNote' = EditNote eName' eDesc' eDate' eSt ePr
        continue (notes, Edit.render eNote', edit yanked eNote')

    handler notes _ = continue (notes, Edit.render eNote, edit yanked eNote)

    toText = either id (T.intercalate (T.pack "\n") . getEditContents)
    note = Note (toText eName) (toText eDesc) (toText eDate) eSt ePr

-- | Search mode handler; maintain a (zipper) of trees that match a regex.
search
  :: [Tree Note]
  -> Maybe (Series (Zipper Note))
  -> Editor T.Text Resource
  -> Handler

search yanked results editor = Handler handler
  where
    normal' = normal yanked

    handler notes (VtyEvent vty@(EvKey key _)) = case key of
      KEnter -> case results of
        Nothing -> search' id
        Just x  -> continue
          ( const (extract x) >>. notes
          , Normal.render
          , normal'
          )
      KEsc   -> continue (notes, Normal.render, normal')
      KUp    -> search' backwards
      KDown  -> search' forwards
      _      -> do
        let query = T.encodeUtf8 . T.concat . getEditContents
            matches eitherPattern text =
              either
              -- If the regex doesn't compile, we fail with IO Bool.
              (pure . const False)
              -- If the regex compiles, we have to execute the regex and check for
              -- a match.
              ( fmap (either (const False) isJust)
              . flip execute (T.encodeUtf8 text)
              ) eitherPattern
            flatten note = T.concat [name note, desc note]
            parse = (uncurry serialize <$>) . uncons
        editor' <- handleEditorEvent vty editor
        regex <- (liftIO . compile compExtended execBlank . query) editor'
        listresults <-
          ( liftIO
          . filterM (matches regex . flatten . extract)
          . list
          . top
          . extract
          ) notes
        continue
          ( notes
          , Search.render (parse listresults) editor'
          , search yanked (parse listresults) editor'
          )
      where
        search' f = continue
          ( notes
          , Search.render (f <$> results) editor
          , search yanked (f <$> results) editor
          )

    handler notes _ = continue
      ( notes
      , Search.render results editor
      , search yanked results editor
      )

-- | Help mode handler; maintain a list of yanked notes.
help :: [Tree Note] -> Handler
help yanked = Handler handler
  where
    handler notes (VtyEvent (EvKey key _)) = case key of
      KEsc      -> continue (notes, Normal.render, normal yanked)
      KChar 'q' -> continue (notes, Normal.render, normal yanked)
      _         -> continue (notes, Help.render,   help yanked)

-- | Leaf node containing a blank note.
emptyNode = Leaf blank
