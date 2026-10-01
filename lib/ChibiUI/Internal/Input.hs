{-# LANGUAGE StrictData #-}

-- | The frame's input, adapted from nano-ui: keys, modifiers, mouse buttons,
-- pointer position, wheel, typed text, and the window size. Positions and
-- window sizes are in logical pixels, scroll in wheel steps, delta time in
-- seconds. Held buttons and keys persist between frames; presses, releases,
-- text and scroll are one-shot events that backends clear with
-- 'clearEphemeral'.
module ChibiUI.Internal.Input
  ( Key (..)
  , Modifiers (..)
  , noModifiers
  , modifiersFromBits
  , modPrimary
  , onMac
  , Input (..)
  , Pressable (..)
  , firstPressed
  , emptyInput
  , inputInteracted
  , inputPointerHeld
  , applyKey
  , releaseAllKeys
  , keypadKey
  , MouseButton (..)
  , mouseButtonNumber
  , MouseButtons
  , noButtons
  , buttonsFromList
  , applyMouseButton
  , applyPointerLeave
  , UiCursorKind (..)
  , syncCursorKind
  , clearEphemeral
  ) where

import Control.Monad (unless, when)
import Data.Bits (Bits, clearBit, countTrailingZeros, setBit, testBit, zeroBits, (.&.))
import Data.IORef (IORef, readIORef, writeIORef)
import Data.List (find)
import Data.Word (Word32)
import System.Info (os)
import ChibiUI.Internal.Types (Size (..), V2 (..))

-- | A key on the keyboard. A key that types a character is 'KeyChar' of the
-- character it types with no modifier held: lower case for a letter, the
-- unshifted symbol otherwise (Shift+1 is @KeyChar \'1\'@ with 'modShift').
-- Presses and releases are reported whatever modifiers are held; any text
-- the key types also arrives in 'inputChars'.
data Key
  = KeyBackspace
  | KeyDelete
  | KeyEnter
  | KeyEscape
  | KeyTab
  | KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyHome
  | KeyEnd
  | KeyPageUp
  | KeyPageDown
  | KeyInsert
  | KeySpace
  | KeyF !Int
  | KeyChar !Char
  deriving (Eq, Ord, Show)

-- | Modifier keys held while the frame's input is processed.
data Modifiers = Modifiers
  { modShift :: !Bool
  , modCtrl :: !Bool
  , modAlt :: !Bool
  , modSuper :: !Bool
  }
  deriving (Eq, Ord, Show)

-- | No modifier held.
noModifiers :: Modifiers
noModifiers = Modifiers False False False False

-- | Read a backend's modifier bit mask, given its bits for Shift, Ctrl, Alt
-- and Super.
{-# INLINE modifiersFromBits #-}
modifiersFromBits :: Bits a => a -> a -> a -> a -> a -> Modifiers
modifiersFromBits m shift ctrl alt super = Modifiers (has shift) (has ctrl) (has alt) (has super)
  where
    has bit = m .&. bit /= zeroBits

-- | Whether the platform's command modifier is held: Command ('modSuper') on
-- macOS, Ctrl elsewhere.
modPrimary :: Modifiers -> Bool
modPrimary = if onMac then modSuper else modCtrl

-- | Whether this is macOS, where Command is the command modifier.
onMac :: Bool
onMac = os == "darwin"
{-# NOINLINE onMac #-}

-- | Input for one frame.
data Input = Input
  { inputMousePos :: {-# UNPACK #-} !V2
  , inputButtonsHeld :: {-# UNPACK #-} !MouseButtons
  , inputButtonsPressed :: {-# UNPACK #-} !MouseButtons
  , inputButtonsReleased :: {-# UNPACK #-} !MouseButtons
  , inputScroll :: {-# UNPACK #-} !V2
  , inputKeys :: [Key]
  -- ^ Keys pressed this frame in event order, auto-repeats included.
  , inputKeysReleased :: [Key]
  , inputKeysHeld :: [Key]
  , inputChars :: [Char]
  -- ^ Typed text this frame, in event order.
  , inputModifiers :: !Modifiers
  , inputWindowSize :: {-# UNPACK #-} !Size
  , inputWindowFocused :: !Bool
  -- ^ Whether the window has keyboard focus; a focused field blinks its
  -- caret only then.
  , inputDeltaTime :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | A key or mouse button: whether it went down this frame, came up, or is
-- held.
--
-- > pressedIn KeyEscape inp
-- > heldIn MouseLeft inp
class Pressable a where
  pressedIn :: a -> Input -> Bool
  releasedIn :: a -> Input -> Bool
  heldIn :: a -> Input -> Bool

instance Pressable Key where
  {-# INLINE pressedIn #-}
  pressedIn k = elem k . inputKeys
  {-# INLINE releasedIn #-}
  releasedIn k = elem k . inputKeysReleased
  {-# INLINE heldIn #-}
  heldIn k = elem k . inputKeysHeld

instance Pressable MouseButton where
  {-# INLINE pressedIn #-}
  pressedIn b = buttonsMember b . inputButtonsPressed
  {-# INLINE releasedIn #-}
  releasedIn b = buttonsMember b . inputButtonsReleased
  {-# INLINE heldIn #-}
  heldIn b = buttonsMember b . inputButtonsHeld

-- | Look this frame's presses up in a key table: the value of the first
-- entry whose key went down, so earlier entries win a tie.
--
-- > firstPressed inp [(KeyUp, -1), (KeyDown, 1)]
firstPressed :: Input -> [(Key, a)] -> Maybe a
firstPressed inp = fmap snd . find ((`pressedIn` inp) . fst)

-- | No events or held buttons, with a focused 800x600 window and zero
-- elapsed time. The native session updates window size, focus and delta
-- time each frame.
emptyInput :: Input
emptyInput =
  Input
    { inputMousePos = V2 0 0
    , inputButtonsHeld = noButtons
    , inputButtonsPressed = noButtons
    , inputButtonsReleased = noButtons
    , inputScroll = V2 0 0
    , inputKeys = []
    , inputKeysReleased = []
    , inputKeysHeld = []
    , inputChars = []
    , inputModifiers = noModifiers
    , inputWindowSize = Size 800 600
    , inputWindowFocused = True
    , inputDeltaTime = 0
    }

-- | Backend-independent cursor shape requested by a hovered control. The
-- shapes are CSS's cursors.
data UiCursorKind
  = UiCursorDefault
  | UiCursorPointer
  | UiCursorText
  | UiCursorHidden
  deriving (Eq, Show, Enum, Bounded)

-- | Show the platform cursor for a kind when it differs from the kind last
-- shown, which @ref@ holds. 'UiCursorHidden' hides the pointer through
-- @setVisible False@ and sets no shape; the next other kind shows it again
-- with @setVisible True@ before @setShape@ sets that kind's shape.
syncCursorKind :: IORef UiCursorKind -> (Bool -> IO ()) -> (UiCursorKind -> IO ()) -> UiCursorKind -> IO ()
syncCursorKind ref setVisible setShape want = do
  cur <- readIORef ref
  when (want /= cur) $ do
    writeIORef ref want
    let hidden = want == UiCursorHidden
    when (hidden /= (cur == UiCursorHidden)) (setVisible (not hidden))
    unless hidden (setShape want)

-- | Clear one-shot events. Keeps held buttons and keys, pointer position,
-- modifiers, window size and delta time.
clearEphemeral :: Input -> Input
clearEphemeral inp =
  inp
    { inputButtonsPressed = noButtons
    , inputButtonsReleased = noButtons
    , inputKeys = []
    , inputKeysReleased = []
    , inputChars = []
    , inputScroll = V2 0 0
    }

-- | Append a key in event order.
{-# INLINE appendInputKey #-}
appendInputKey :: Key -> [Key] -> [Key]
appendInputKey k ks = ks ++ [k]

-- | Apply a key press ('True') or release. A press joins 'inputKeys', and
-- also 'inputKeysHeld' unless the key is already held (an auto-repeat). A
-- release joins 'inputKeysReleased' and leaves the held keys. Backends pass
-- auto-repeats in as presses.
applyKey :: Key -> Bool -> Input -> Input
applyKey k True inp =
  inp
    { inputKeys = appendInputKey k (inputKeys inp)
    , inputKeysHeld = if k `elem` held then held else appendInputKey k held
    }
  where
    held = inputKeysHeld inp
applyKey k False inp =
  inp
    { inputKeysReleased = appendInputKey k (inputKeysReleased inp)
    , inputKeysHeld = filter (/= k) (inputKeysHeld inp)
    }

-- | Release every held key and modifier. Backends call this when the window
-- loses keyboard focus, since keys released elsewhere send no release event.
releaseAllKeys :: Input -> Input
releaseAllKeys inp =
  inp
    { inputKeysReleased = inputKeysReleased inp ++ inputKeysHeld inp
    , inputKeysHeld = []
    , inputModifiers = noModifiers
    }

-- | The key for a keypad digit or point (@'0'@ to @'9'@, @'.'@): the typed
-- character with Num Lock on, otherwise the navigation key printed on it
-- ('Nothing' for 5).
keypadKey :: Bool -> Char -> Maybe Key
keypadKey True c = Just (KeyChar c)
keypadKey False c =
  lookup c [('0', KeyInsert), ('1', KeyEnd), ('2', KeyDown), ('3', KeyPageDown), ('4', KeyLeft), ('6', KeyRight), ('7', KeyHome), ('8', KeyUp), ('9', KeyPageUp), ('.', KeyDelete)]

-- | A mouse button. 'MouseBack' and 'MouseForward' are the side buttons (X1
-- and X2) that browsers use for navigation.
data MouseButton
  = MouseLeft
  | MouseRight
  | MouseMiddle
  | MouseBack
  | MouseForward
  | MouseOther !Int
  deriving (Eq, Ord, Show)

-- | The button for a 1-based platform button number.
mouseButtonNumber :: Int -> MouseButton
mouseButtonNumber = \case
  1 -> MouseLeft
  2 -> MouseMiddle
  3 -> MouseRight
  4 -> MouseBack
  5 -> MouseForward
  n -> MouseOther n

-- | The button's bit in 'MouseButtons': its number minus one, or -1 past 32.
{-# INLINE buttonBit #-}
buttonBit :: MouseButton -> Int
buttonBit = \case
  MouseLeft -> 0
  MouseMiddle -> 1
  MouseRight -> 2
  MouseBack -> 3
  MouseForward -> 4
  MouseOther n
    | n >= 1 && n <= 32 -> n - 1
    | otherwise -> -1

-- | A set of mouse buttons, as held, pressed or released in an 'Input'.
newtype MouseButtons = MouseButtons Word32
  deriving (Eq)

instance Show MouseButtons where
  showsPrec d bs = showParen (d > 10) (showString "buttonsFromList " . showsPrec 11 (buttonsToList bs))

-- | No button.
noButtons :: MouseButtons
noButtons = MouseButtons 0

-- | Whether the set holds the button.
{-# INLINE buttonsMember #-}
buttonsMember :: MouseButton -> MouseButtons -> Bool
buttonsMember b (MouseButtons w) = let i = buttonBit b in i >= 0 && testBit w i

-- | Whether the set is empty.
{-# INLINE buttonsNull #-}
buttonsNull :: MouseButtons -> Bool
buttonsNull (MouseButtons w) = w == 0

-- | The set with the button added.
{-# INLINE buttonsInsert #-}
buttonsInsert :: MouseButton -> MouseButtons -> MouseButtons
buttonsInsert b bs@(MouseButtons w) = let i = buttonBit b in if i < 0 then bs else MouseButtons (setBit w i)

-- | The set without the button.
{-# INLINE buttonsDelete #-}
buttonsDelete :: MouseButton -> MouseButtons -> MouseButtons
buttonsDelete b bs@(MouseButtons w) = let i = buttonBit b in if i < 0 then bs else MouseButtons (clearBit w i)

-- | The buttons in the set, by number.
buttonsToList :: MouseButtons -> [MouseButton]
buttonsToList (MouseButtons w)
  | w == 0 = []
  | otherwise =
      let i = countTrailingZeros w
       in mouseButtonNumber (i + 1) : buttonsToList (MouseButtons (clearBit w i))

-- | The set of the buttons listed.
buttonsFromList :: [MouseButton] -> MouseButtons
buttonsFromList = foldl' (flip buttonsInsert) noButtons

-- | Apply a button press ('True') or release to the held, pressed and
-- released sets.
applyMouseButton :: MouseButton -> Bool -> Input -> Input
applyMouseButton b True inp =
  inp {inputButtonsHeld = buttonsInsert b (inputButtonsHeld inp), inputButtonsPressed = buttonsInsert b (inputButtonsPressed inp)}
applyMouseButton b False inp =
  inp {inputButtonsHeld = buttonsDelete b (inputButtonsHeld inp), inputButtonsReleased = buttonsInsert b (inputButtonsReleased inp)}

-- | The pointer left the window: move it off every widget, so nothing stays
-- hovered. Held buttons stay held until their releases arrive.
applyPointerLeave :: Input -> Input
applyPointerLeave inp = inp {inputMousePos = offWindow}

-- | A point far outside any window.
offWindow :: V2
offWindow = V2 (-1e6) (-1e6)

-- | Compare interaction fields, including buttons, keys, scroll and window
-- size. Pointer motion and elapsed time are ignored.
inputInteracted :: Input -> Input -> Bool
inputInteracted a b = quiet a /= quiet b
  where
    quiet i = i {inputMousePos = V2 0 0, inputDeltaTime = 0}

-- | Whether any mouse button is held.
{-# INLINE inputPointerHeld #-}
inputPointerHeld :: Input -> Bool
inputPointerHeld = not . buttonsNull . inputButtonsHeld
