-- | RGFW imports and header-derived event accessors and constants. The C
-- compiler supplies field offsets and enum values from the bundled header.
module RGFW.Raw
  ( RGFW_window
  , RGFW_event
  , c_RGFW_window_close
  , c_RGFW_window_checkEvent
  , c_RGFW_waitForEvent
  , c_rgfw_create_window_gl
  , c_RGFW_window_swapBuffers_OpenGL
  , c_RGFW_window_showMouse
  , c_rgfw_event_type
  , c_rgfw_event_mouse_x
  , c_rgfw_event_mouse_y
  , c_rgfw_event_button_value
  , c_rgfw_event_delta_x
  , c_rgfw_event_delta_y
  , c_rgfw_event_key_value
  , c_rgfw_event_key_mod
  , c_rgfw_event_keyChar_value
  , c_rgfw_event_size
  , c_rgfw_window_w
  , c_rgfw_window_h
  , c_rgfw_window_scale
  , c_rgfw_read_clipboard_text
  , c_rgfw_write_clipboard_text
  -- Event types
  , rgfw_keyPressed
  , rgfw_keyReleased
  , rgfw_keyChar
  , rgfw_mouseButtonPressed
  , rgfw_mouseButtonReleased
  , rgfw_mouseScroll
  , rgfw_mouseMotion
  , rgfw_mouseLeave
  , rgfw_windowFocusOut
  , rgfw_windowClose
  -- Keys
  , rgfw_keyBackSpace
  , rgfw_keyTab
  , rgfw_keyReturn
  , rgfw_keyEscape
  , rgfw_keyDelete
  , rgfw_keyUp
  , rgfw_keyDown
  , rgfw_keyLeft
  , rgfw_keyRight
  , rgfw_keyEnd
  , rgfw_keyHome
  , rgfw_keySpace
  , rgfw_keyInsert
  , rgfw_keyPageUp
  , rgfw_keyPageDown
  , rgfw_keyF1
  , rgfw_keyF24
  , rgfw_keyPad0
  , rgfw_keyPad1
  , rgfw_keyPad2
  , rgfw_keyPad3
  , rgfw_keyPad4
  , rgfw_keyPad5
  , rgfw_keyPad6
  , rgfw_keyPad7
  , rgfw_keyPad8
  , rgfw_keyPad9
  , rgfw_keyPadPeriod
  , rgfw_keyPadReturn
  -- Key modifier bits
  , rgfw_modNumLock
  , rgfw_modControl
  , rgfw_modAlt
  , rgfw_modShift
  , rgfw_modSuper
  -- Window flags
  , rgfw_windowNoResize
  , rgfw_windowFullscreen
  -- Window options and state
  , c_RGFW_window_center
  , c_RGFW_window_resize
  , c_RGFW_window_show
  , c_RGFW_window_isInFocus
  -- Mouse cursors
  , c_rgfw_window_set_mouse_standard
  , c_rgfw_window_set_mouse_default
  , rgfw_mouseIbeam
  , rgfw_mousePointingHand
  ) where

import Data.Word (Word8, Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CFloat (..), CInt (..), CSize (..), CUChar (..), CUInt (..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)

#include "RGFW.h"

-- | Opaque native window. The creating thread owns its lifetime.
data RGFW_window
-- | Native tagged event union. Allocate 'c_rgfw_event_size' bytes and read
-- only the fields valid for the tag returned by 'c_rgfw_event_type'.
data RGFW_event

-- Foreign function imports
-- | Destroy a live window and its OpenGL context. Do not reuse the pointer.
foreign import ccall "RGFW_window_close"
  c_RGFW_window_close :: Ptr RGFW_window -> IO ()

-- | Poll into caller-owned event storage. Zero means no event was written.
foreign import ccall unsafe "RGFW_window_checkEvent"
  c_RGFW_window_checkEvent :: Ptr RGFW_window -> Ptr RGFW_event -> IO CUChar

-- | Wait in milliseconds: negative blocks indefinitely; zero does not wait.
foreign import ccall "RGFW_waitForEvent"
  c_RGFW_waitForEvent :: CInt -> IO ()

-- | Create title/x/y/width/height/flags with a core OpenGL major/minor version.
-- Returns null on failure; the resulting context is current on this OS thread.
foreign import ccall "rgfw_create_window_gl"
  c_rgfw_create_window_gl :: CString -> CInt -> CInt -> CInt -> CInt -> CUInt -> CInt -> CInt -> IO (Ptr RGFW_window)

-- | Show the pointer over the window (non-zero) or hide it.
foreign import ccall "RGFW_window_showMouse"
  c_RGFW_window_showMouse :: Ptr RGFW_window -> CUChar -> IO ()

-- | Present the window's OpenGL back buffer on the context's owning thread.
foreign import ccall "RGFW_window_swapBuffers_OpenGL"
  c_RGFW_window_swapBuffers_OpenGL :: Ptr RGFW_window -> IO ()

-- Event accessors read only the union member selected by the event tag.

-- | Event tag; inspect it before reading fields of the event union.
c_rgfw_event_type :: Ptr RGFW_event -> IO CUChar
c_rgfw_event_type = #{peek RGFW_event, type}

-- | Mouse-motion x coordinate in native window pixels.
c_rgfw_event_mouse_x :: Ptr RGFW_event -> IO CInt
c_rgfw_event_mouse_x = #{peek RGFW_event, mouse.x}

-- | Mouse-motion y coordinate in native window pixels.
c_rgfw_event_mouse_y :: Ptr RGFW_event -> IO CInt
c_rgfw_event_mouse_y = #{peek RGFW_event, mouse.y}

-- | Button code from a mouse-button event: left, middle, right, then the
-- side buttons.
c_rgfw_event_button_value :: Ptr RGFW_event -> IO CUChar
c_rgfw_event_button_value = #{peek RGFW_event, button.value}

-- | Horizontal wheel delta from a scroll event.
c_rgfw_event_delta_x :: Ptr RGFW_event -> IO CFloat
c_rgfw_event_delta_x = #{peek RGFW_event, delta.x}

-- | Vertical wheel delta from a scroll event.
c_rgfw_event_delta_y :: Ptr RGFW_event -> IO CFloat
c_rgfw_event_delta_y = #{peek RGFW_event, delta.y}

-- | RGFW key code from a key press or release, not a Unicode text character.
c_rgfw_event_key_value :: Ptr RGFW_event -> IO CUInt
c_rgfw_event_key_value p = fromIntegral <$> (#{peek RGFW_event, key.value} p :: IO #{type RGFW_key})

-- | Modifier bit mask from a key event; test with the @rgfw_mod*@ constants.
c_rgfw_event_key_mod :: Ptr RGFW_event -> IO CUChar
c_rgfw_event_key_mod = #{peek RGFW_event, key.mod}

-- | Code point from a character event. Validate it before converting to 'Char'.
c_rgfw_event_keyChar_value :: Ptr RGFW_event -> IO CUInt
c_rgfw_event_keyChar_value = #{peek RGFW_event, keyChar.value}

-- | Native event structure size in bytes, for allocating event storage.
c_rgfw_event_size :: IO CSize
c_rgfw_event_size = pure #{size RGFW_event}

-- | Current native window width in pixels. Requires a live non-null pointer.
foreign import ccall unsafe "rgfw_window_w"
  c_rgfw_window_w :: Ptr RGFW_window -> IO CInt

-- | Current native window height in pixels. Requires a live non-null pointer.
foreign import ccall unsafe "rgfw_window_h"
  c_rgfw_window_h :: Ptr RGFW_window -> IO CInt

-- | Monitor's horizontal display scale, falling back to 1 if unavailable.
foreign import ccall unsafe "rgfw_window_scale"
  c_rgfw_window_scale :: Ptr RGFW_window -> IO CFloat

-- Clipboard (cbits/RGFW.c)
-- | Borrow clipboard UTF-8 until the next clipboard read. Writes the byte
-- length, excluding trailing NULs, to the supplied pointer. Null means no text.
foreign import ccall "rgfw_read_clipboard_text"
  c_rgfw_read_clipboard_text :: Ptr CSize -> IO CString

-- | Copy UTF-8 bytes to the clipboard. Length is in bytes; zero is allowed.
-- Returns zero on failure. The buffer only needs to live through the call.
foreign import ccall "rgfw_write_clipboard_text"
  c_rgfw_write_clipboard_text :: CString -> CSize -> IO CUChar

-- | Event tags for key press, key release, and typed character.
rgfw_keyPressed, rgfw_keyReleased, rgfw_keyChar :: Word8
-- | Event tags for button press/release, wheel movement, pointer movement,
-- and the pointer leaving the window.
rgfw_mouseButtonPressed, rgfw_mouseButtonReleased, rgfw_mouseScroll, rgfw_mouseMotion, rgfw_mouseLeave :: Word8
-- | Event tags for losing focus and close requests.
rgfw_windowFocusOut, rgfw_windowClose :: Word8
rgfw_keyPressed          = #{const RGFW_keyPressed}
rgfw_keyReleased         = #{const RGFW_keyReleased}
rgfw_keyChar             = #{const RGFW_keyChar}
rgfw_mouseButtonPressed  = #{const RGFW_mouseButtonPressed}
rgfw_mouseButtonReleased = #{const RGFW_mouseButtonReleased}
rgfw_mouseScroll         = #{const RGFW_mouseScroll}
rgfw_mouseMotion         = #{const RGFW_mouseMotion}
rgfw_mouseLeave          = #{const RGFW_mouseLeave}
rgfw_windowFocusOut      = #{const RGFW_windowFocusOut}
rgfw_windowClose         = #{const RGFW_windowClose}

-- | Editing key codes carried by key events.
rgfw_keyBackSpace, rgfw_keyTab, rgfw_keyReturn, rgfw_keyEscape, rgfw_keyDelete :: Word32
-- | Navigation key codes carried by key events.
rgfw_keyUp, rgfw_keyDown, rgfw_keyLeft, rgfw_keyRight, rgfw_keyEnd, rgfw_keyHome :: Word32
rgfw_keyBackSpace = #{const RGFW_keyBackSpace}
rgfw_keyTab       = #{const RGFW_keyTab}
rgfw_keyReturn    = #{const RGFW_keyReturn}
rgfw_keyEscape    = #{const RGFW_keyEscape}
rgfw_keyDelete    = #{const RGFW_keyDelete}
rgfw_keyUp        = #{const RGFW_keyUp}
rgfw_keyDown      = #{const RGFW_keyDown}
rgfw_keyLeft      = #{const RGFW_keyLeft}
rgfw_keyRight     = #{const RGFW_keyRight}
rgfw_keyEnd       = #{const RGFW_keyEnd}
rgfw_keyHome      = #{const RGFW_keyHome}

-- | More named key codes. 'rgfw_keyF1' to 'rgfw_keyF24' are consecutive, as
-- are 'rgfw_keyPad1' to 'rgfw_keyPad9'.
rgfw_keySpace, rgfw_keyInsert, rgfw_keyPageUp, rgfw_keyPageDown, rgfw_keyF1, rgfw_keyF24 :: Word32
rgfw_keyPad0, rgfw_keyPad1, rgfw_keyPad2, rgfw_keyPad3, rgfw_keyPad4, rgfw_keyPad5, rgfw_keyPad6, rgfw_keyPad7, rgfw_keyPad8, rgfw_keyPad9 :: Word32
rgfw_keyPadPeriod, rgfw_keyPadReturn :: Word32
rgfw_keySpace       = #{const RGFW_keySpace}
rgfw_keyInsert      = #{const RGFW_keyInsert}
rgfw_keyPageUp      = #{const RGFW_keyPageUp}
rgfw_keyPageDown    = #{const RGFW_keyPageDown}
rgfw_keyF1          = #{const RGFW_keyF1}
rgfw_keyF24         = #{const RGFW_keyF24}
rgfw_keyPad0        = #{const RGFW_keyPad0}
rgfw_keyPad1        = #{const RGFW_keyPad1}
rgfw_keyPad2        = #{const RGFW_keyPad2}
rgfw_keyPad3        = #{const RGFW_keyPad3}
rgfw_keyPad4        = #{const RGFW_keyPad4}
rgfw_keyPad5        = #{const RGFW_keyPad5}
rgfw_keyPad6        = #{const RGFW_keyPad6}
rgfw_keyPad7        = #{const RGFW_keyPad7}
rgfw_keyPad8        = #{const RGFW_keyPad8}
rgfw_keyPad9        = #{const RGFW_keyPad9}
rgfw_keyPadPeriod   = #{const RGFW_keyPadPeriod}
rgfw_keyPadReturn   = #{const RGFW_keyPadReturn}

-- | Independent modifier bits, combined with bitwise OR in key events.
rgfw_modNumLock, rgfw_modControl, rgfw_modAlt, rgfw_modShift, rgfw_modSuper :: Word8
rgfw_modNumLock    = #{const RGFW_modNumLock}
rgfw_modControl    = #{const RGFW_modControl}
rgfw_modAlt        = #{const RGFW_modAlt}
rgfw_modShift      = #{const RGFW_modShift}
rgfw_modSuper      = #{const RGFW_modSuper}

-- | Window creation flags: not user-resizable, and fullscreen.
rgfw_windowNoResize, rgfw_windowFullscreen :: Word32
rgfw_windowNoResize   = #{const RGFW_windowNoResize}
rgfw_windowFullscreen = #{const RGFW_windowFullscreen}

-- Window options

-- | Centre the window on its monitor.
foreign import ccall "RGFW_window_center"
  c_RGFW_window_center :: Ptr RGFW_window -> IO ()

-- | Resize the window to a size in native pixels.
foreign import ccall "RGFW_window_resize"
  c_RGFW_window_resize :: Ptr RGFW_window -> CInt -> CInt -> IO ()

-- | Show a hidden window.
foreign import ccall "RGFW_window_show"
  c_RGFW_window_show :: Ptr RGFW_window -> IO ()

-- | Whether the window has keyboard focus, as cached by RGFW from events.
foreign import ccall unsafe "RGFW_window_isInFocus"
  c_RGFW_window_isInFocus :: Ptr RGFW_window -> IO CUChar

-- Mouse cursors

-- | Set a standard cursor shape. Returns zero on failure or for a null window.
foreign import ccall "rgfw_window_set_mouse_standard"
  c_rgfw_window_set_mouse_standard :: Ptr RGFW_window -> CUChar -> IO CUChar

-- | Restore the native default cursor. Returns zero on failure.
foreign import ccall "rgfw_window_set_mouse_default"
  c_rgfw_window_set_mouse_default :: Ptr RGFW_window -> IO CUChar

-- | Standard cursor codes for text and link cursors.
rgfw_mouseIbeam, rgfw_mousePointingHand :: Word8
rgfw_mouseIbeam        = #{const RGFW_mouseIbeam}
rgfw_mousePointingHand = #{const RGFW_mousePointingHand}
