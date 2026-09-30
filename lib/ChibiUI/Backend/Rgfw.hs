-- | Run chibi-ui views in an RGFW window with OpenGL 3.2, rasterizing
-- text with the bundled RFont from an embedded TrueType Inter subset
-- (or @optFontPath@'s TTF). 'runChibiApp' runs a view until the window
-- closes or the view calls 'ChibiUI.quitUi'.
module ChibiUI.Backend.Rgfw
  ( runChibiApp
  , RgfwOptions (..)
  , defaultRgfwOptions
  , WindowSettings (..)
  , defaultWindowSettings
  ) where

import ChibiUI.Internal.Monad (ChibiUI)
import ChibiUI.Rgfw.Internal.Session
  ( RgfwOptions (..)
  , WindowSettings (..)
  , defaultRgfwOptions
  , defaultWindowSettings
  , runChibiAppWith
  )

-- | Run a view with an initial application model and @opts@' window,
-- theme, and scale. Model updates persist across frames.
runChibiApp :: RgfwOptions -> model -> ChibiUI model () -> IO ()
runChibiApp = runChibiAppWith
