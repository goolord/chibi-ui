-- | Small command-based text editor shared by fields and text areas.
-- Character offsets, selection and bounded snapshots; no document tree.
module ChibiUI.Internal.Editor
  ( Editor (..), EditState (..), Command (..), Motion (..)
  , newEditor, selection, selectedText, select, selectWord, replace, command, inputCommands
  , textLines, caretLine, sanitizeText
  ) where

import Data.Char (isAlphaNum, isPrint, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import System.Info (os)
import ChibiUI.Internal.Input

data EditState = EditState {editText :: !Text, editCaret :: !Int, editAnchor :: !Int}
  deriving (Eq, Show)

data Editor = Editor
  { editState :: !EditState
  , editUndo :: ![EditState]
  , editRedo :: ![EditState]
  } deriving (Eq, Show)

data Motion = Backward | Forward | WordBackward | WordForward | Start | End
  | LineStart | LineEnd | PreviousLine | NextLine
  deriving (Eq, Show)

data Command = Insert !Text | Move !Motion !Bool | Delete !Motion
  | SelectAll | Copy | Cut | Paste | Undo | Redo
  deriving (Eq, Show)

newEditor :: Text -> Editor
newEditor t = Editor (EditState t (T.length t) (T.length t)) [] []

selection :: Editor -> (Int, Int)
selection ed = let EditState _ c a = editState ed in (min c a, max c a)

selectedText :: Editor -> Text
selectedText ed = let (a, b) = selection ed in T.take (b - a) (T.drop a (editText (editState ed)))

select :: Int -> Int -> Editor -> Editor
select a c ed = ed {editState = s {editCaret = limit c, editAnchor = limit a}}
  where
    s = editState ed
    limit = max 0 . min (T.length (editText s))

selectWord :: Int -> Editor -> Editor
selectWord index ed
  | T.null t = ed
  | otherwise = select (i - T.length (T.takeWhileEnd same (T.take i t)))
      (i + T.length (T.takeWhile same (T.drop i t))) ed
  where
    t = editText (editState ed)
    i = max 0 (min (T.length t - 1) index)
    same c = wordCategory c == wordCategory (T.index t i)

-- Selection and word motion agree on whitespace, identifiers and punctuation.
wordCategory :: Char -> Int
wordCategory c
  | isSpace c = 0
  | isAlphaNum c || c == '_' = 1
  | otherwise = 2

replace :: Text -> Editor -> Editor
replace raw ed =
  let s = editState ed
      (a, b) = selection ed
      text = sanitizeText True raw
      t = T.take a (editText s) <> text <> T.drop b (editText s)
      c = a + T.length text
   in ed {editState = EditState t c c,
          editUndo = if t == editText s then editUndo ed else remember s (editUndo ed),
          editRedo = if t == editText s then editRedo ed else []}

-- Force the bounded spine so lazy take thunks cannot retain older history.
remember :: EditState -> [EditState] -> [EditState]
remember s history = let kept = take 100 (s : history) in length kept `seq` kept

-- | Logical lines with character offsets, including a trailing empty line.
textLines :: Text -> [(Int, Text)]
textLines t = zip (scanl (\n line -> n + T.length line + 1) 0 ls) ls
  where ls = T.splitOn "\n" t

caretLine :: Text -> Int -> (Int, Text)
caretLine t c = last (takeWhile ((<= max 0 c) . fst) (textLines t))

-- | Fields discard controls; text areas retain LF and expand pasted tabs.
sanitizeText :: Bool -> Text -> Text
sanitizeText False = T.filter isPrint
sanitizeText True = T.filter (\c -> isPrint c || c == '\n')
  . T.replace "\t" "    " . T.replace "\r" "\n" . T.replace "\r\n" "\n"

target :: Motion -> EditState -> Int
target motion (EditState t c _) = case motion of
  Backward -> max 0 (c - 1)
  Forward -> min (T.length t) (c + 1)
  Start -> 0
  End -> T.length t
  WordBackward -> c - wordLength (reverse (T.unpack (T.take c t)))
  WordForward -> c + wordLength (T.unpack (T.drop c t))
  LineStart -> start
  LineEnd -> start + T.length line
  PreviousLine -> vertical (-1)
  NextLine -> vertical 1
  where
    (start, line) = caretLine t c
    vertical delta =
      let ls = textLines t
          row = T.count "\n" (T.take c t)
          (offset, destination) = ls !! max 0 (min (length ls - 1) (row + delta))
       in offset + min (c - start) (T.length destination)
    wordLength xs =
      let (spaces, rest) = span isSpace xs
       in length spaces + case rest of
            [] -> 0
            x : _ -> length (takeWhile ((== wordCategory x) . wordCategory) rest)

-- | Clipboard commands are handled by the widget, then use 'Insert'/'Delete'.
command :: Command -> Editor -> Editor
command cmd ed = case cmd of
  Insert t -> replace t ed
  SelectAll -> select 0 (T.length (editText s)) ed
  Move m extend ->
    let c | not extend && a /= b && m == Backward = a
          | not extend && a /= b && m == Forward = b
          | otherwise = target m s
     in select (if extend then editAnchor s else c) c ed
  Delete m ->
    let result = replace "" (if a /= b then ed else select (editCaret s) (target m s) ed)
     in if editText (editState result) == editText s then result
        else result {editUndo = remember s (editUndo ed)}
  Undo -> case editUndo ed of
    x : xs -> Editor x xs (s : editRedo ed)
    [] -> ed
  Redo -> case editRedo ed of
    x : xs -> Editor x (s : editUndo ed) xs
    [] -> ed
  _ -> ed
  where
    s = editState ed
    (a, b) = selection ed

inputCommands :: Bool -> Input -> [Command]
inputCommands multiline inp = concatMap binding (inputKeys inp) ++ typing
  where
    mods = inputModifiers inp
    primary = modPrimary mods && not (modAlt mods)
    shift = modShift mods
    jump = if os == "darwin" then modAlt mods else modCtrl mods
    macCommand = os == "darwin" && modSuper mods
    lineStart = if multiline then LineStart else Start
    lineEnd = if multiline then LineEnd else End
    left = if macCommand then lineStart else if jump then WordBackward else Backward
    right = if macCommand then lineEnd else if jump then WordForward else Forward
    binding k = case k of
      KeyChar 'a' | primary -> [SelectAll]
      KeyChar 'c' | primary -> [Copy]
      KeyChar 'x' | primary -> [Cut]
      KeyChar 'v' | primary -> [Paste]
      KeyChar 'z' | primary -> [if shift then Redo else Undo]
      KeyChar 'y' | primary -> [Redo]
      KeyLeft -> [Move left shift]
      KeyRight -> [Move right shift]
      KeyHome -> [Move (if primary then Start else lineStart) shift]
      KeyEnd -> [Move (if primary then End else lineEnd) shift]
      KeyUp | multiline -> [Move PreviousLine shift]
      KeyDown | multiline -> [Move NextLine shift]
      KeyEnter | multiline -> [Insert "\n"]
      KeyBackspace -> [Delete left]
      KeyDelete -> [Delete right]
      _ -> []
    text = sanitizeText multiline (T.pack (inputChars inp))
    -- Option-composed characters and Windows AltGr are text, not app chords.
    textModifiers = not (modSuper mods) && (not (modCtrl mods) || modAlt mods)
    typing = [Insert text | not (T.null text), textModifiers]
