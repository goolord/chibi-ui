-- | Small command-based text editor shared by fields and text areas.
-- Character offsets, selection and bounded snapshots; no document tree.
module ChibiUI.Internal.Editor
  ( Editor (..), EditState (..), Command (..), Motion (..)
  , newEditor, selection, selectedText, select, selectWord, replace, command, inputCommands
  , textLines, caretRowLine, sanitizeText
  ) where

import Data.Char (isAlphaNum, isPrint, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import ChibiUI.Internal.Input

data EditState = EditState {editText :: !Text, editCaret :: !Int, editAnchor :: !Int}
  deriving (Eq, Show)

-- | Each undo step keeps the whole text (a 'Text' edit copies), so a
-- history entry also carries its byte size, cached when the entry is
-- pushed. 'remember' can then bound what a field retains without walking
-- old texts again.
type HistoryEntry = (EditState, Int)

data Editor = Editor
  { editState :: !EditState
  , editUndo :: ![HistoryEntry]
  , editRedo :: ![HistoryEntry]
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

-- | Push an undo step, bounded two ways: at most 'undoDepth' states, and
-- at most 'undoBudget' bytes of retained text across them, so editing a
-- large document keeps a working undo without holding unbounded copies.
-- The budget drops the oldest states; sizes are cached per entry, so
-- pushing is list arithmetic, not text walking. The spine is forced so
-- lazy takes cannot retain older history.
remember :: EditState -> [HistoryEntry] -> [HistoryEntry]
remember s history =
  let kept = take undoDepth (entryOf s : history)
      retained = scanl (+) (snd (entryOf s)) (map snd history)
      withinBudget = [e | (c, e) <- zip retained kept, c <= undoBudget]
   in length withinBudget `seq` withinBudget

-- | An undo step with its cached text size.
entryOf :: EditState -> HistoryEntry
entryOf s = (s, TU.lengthWord8 (editText s))

-- | Undo steps kept per field.
undoDepth :: Int
undoDepth = 100

-- | Total edited text a field's undo history may retain, in bytes.
undoBudget :: Int
undoBudget = 262144

-- | Logical lines with character offsets, including a trailing empty line.
textLines :: Text -> [(Int, Text)]
textLines t = zip (scanl (\n line -> n + T.length line + 1) 0 ls) ls
  where ls = T.splitOn "\n" t

-- | The caret's row among 'textLines' and that line, as
-- @(row, (start, line))@.
caretRowLine :: [(Int, Text)] -> Int -> (Int, (Int, Text))
caretRowLine ls c =
  let before = takeWhile ((<= max 0 c) . fst) ls
   in (length before - 1, last before)

-- | Fields discard controls; text areas retain LF and expand pasted tabs.
-- Clean text, the usual case, comes back as is after one scan.
sanitizeText :: Bool -> Text -> Text
sanitizeText False t
  | T.all isPrint t = t
  | otherwise = T.filter isPrint t
sanitizeText True t
  | T.all kept t = t
  | otherwise =
      T.filter kept . T.replace "\t" "    " . T.replace "\r" "\n" . T.replace "\r\n" "\n" $ t
  where
    kept c = isPrint c || c == '\n'

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
    ls = textLines t
    (row, (start, line)) = caretRowLine ls c
    vertical delta =
      let (offset, destination) = ls !! max 0 (min (length ls - 1) (row + delta))
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
    (x, _) : xs -> Editor x xs (entryOf s : editRedo ed)
    [] -> ed
  Redo -> case editRedo ed of
    (x, _) : xs -> Editor x (entryOf s : editUndo ed) xs
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
    jump = if onMac then modAlt mods else modCtrl mods
    macCommand = onMac && modSuper mods

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
