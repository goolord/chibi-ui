# Change log

## Unreleased

- Added `disabled flag body`: while the flag holds, the body's widgets draw
  dimmed and ignore the pointer and keyboard; Tab skips them and their
  context menus stay shut.

- Added `tooltip text`: once the pointer has rested on the preceding
  widget or group for half a second, the text shows beside the pointer,
  over every widget. It waits with `requestFrameAt`, not by polling.

- Added last-item queries: `itemRect`, `itemHovered`, `itemFocused` and
  `itemActive` ask about the widget or group placed just before, as
  `contextMenu` attaches to it.

- Added `tabs titles selected`: a row of tab headers that selects the one
  clicked, for the caller to show its page.

- Added `combo options value`: a field showing the chosen option that
  opens a menu of the options.

- Added `progressBar fraction`: a bar filled that far across.

- Added `radio options value`: a row of boxes, one per option, that
  chooses the one clicked.

- Added `checkbox caption value`: a box that a click, or Enter/Space
  while focused, flips.

- `scrollColumn`'s scrollbar takes the mouse: drag the thumb to scroll,
  or press the track to jump the thumb under the pointer and keep
  dragging. The drag holds the pointer until release, even off the bar,
  and the thumb brightens while hovered or dragged.

- Flat geometry and text share one batch: flat quads sample the atlas
  with a UV of -1, which reads as full coverage, so the demo draws in 3
  calls instead of 36. The renderer keeps a fixed index buffer, and the
  draw list holds vertices only. `Backend` changes: `texFlat` and
  `texGlyphAtlas` become `texAtlas` (id 0), image ids start at 1,
  `DrawCmd` counts quads (`cmdFirstQuad`, `cmdQuadCount`), and `DrawData`
  loses `drawIndices` and `drawIndexCount`.

- `trackFrame` copies each frame into the snapshot it replaces instead
  of allocating a new one, and records which quads changed
  (`snapshotChangedQuads`); the RGFW renderer re-uploads only those when
  the quad count holds.

- New glyphs upload just the atlas rows they landed in, and no longer
  force a full repaint: packing only appends, so no existing quad samples
  different texels. `fontTakeDirty` reports an `AtlasChange`.

- Plots merge every segment crossing a pixel column into one quad, so a
  line costs at most a quad per pixel of width however many samples it
  has (the demo's plot: 58 fewer quads).

- App keys (`keyPressed`, `keyHeld`, `shortcut`, `primaryShortcut`) are
  suppressed only while a text field is focused, not while any widget is:
  a clicked button no longer swallows Ctrl+S. Tables take focus and Tab,
  and Up/Down move their selection. Shift+F10 opens a `contextMenu` when
  the focused widget lies inside the widget or group it is attached to;
  the first menu declared wins. `scrollColumn` fills the rest of its
  scope's height, so it sizes correctly inside groups, takes `nextHeight`,
  and nests. Plots draw one quad per pixel column instead of one per pixel
  of line length.

- Added `requestFrameAt`: ask for a frame once `uiTime` reaches a time. The
  caret blink uses it, so a focused field wakes the loop twice a second
  instead of thirty times, and only while the window has focus (new
  `inputWindowFocused`).

- Draw commands batch by texture alone: quads are already cut to their
  clip as they are emitted, so `DrawCmd` loses its clip fields and a frame
  draws in far fewer calls (the demo: 49 to 36). Removed `cursorFallback`
  and `MouseButtons`' `Semigroup`/`Monoid` instances.

- Added `slider value lo hi`: a horizontal slider you drag or click, with
  Left/Right steps by a tenth of the range while focused. Added `plotLines`:
  a line plot of a list of values, auto-scaled to its lowest and highest
  samples.

- Text renders like nano-ui's instead of blocky, uneven strokes, and the
  default font size is 16 px, up from 13 (nano-ui's default). Glyph quads
  now land on whole device pixels: the rasterizer bakes each glyph once at
  integer positions, but the pen placed quads fractionally (the baseline
  always is fractional), so nearest-texel sampling dropped and duplicated
  coverage columns. The baseline snaps per line and each glyph's pen per
  glyph, in device space, the way terminals place glyphs; advances still
  accumulate fractionally, so measurement and hit testing are unchanged.
  The glyph atlas samples with linear filtering (nano-ui does the same),
  with one texel of padding after every glyph and every atlas row so
  filtering cannot bleed a neighbour glyph into an edge sample; quads that
  land on whole pixels still sample exact texels, since pixel centers hit
  texel centers. Glyph advances are rounded to device pixels instead of
  truncated, so letter spacing no longer runs up to a pixel per character
  tighter than the font designs. The atlas packer's larger cells still
  hold the 256-glyph font subset many times over at the default size.

- Cut steady-state allocation roughly in half again (demo view idle
  frames: about 200 KB to about 90 KB per frame; motion and typing frames
  by similar margins; minor GCs per run: 71 to 29), with identical
  geometry, damage decisions, and widget behavior. The draw arena now
  tracks its open batch in unboxed counters, so a quad that continues its
  batch allocates nothing: command closure, index-count boxes, and the
  per-quad keep-alive are gone, with base pointers cached between
  growths. Widget ids advance two plain counters instead of rebuilding an
  id-context record per widget. Per-frame widget rects moved from a
  rebuilt `IntMap` to a reusable open-addressing table the context clears
  in place, which also stops the whole previous map becoming garbage each
  frame. Field and scroll state slots skip writes whose value did not
  change, layout bounds unboxed into the cursor state, damage batch
  comparison streams field-wise without building tuple lists, and the
  library builds at `-O2`. Undo history is now bounded not just to 100
  states but to 256 KB of retained text per field, with sizes cached per
  entry so pushing stays list arithmetic.
- Reduced per-frame allocation churn roughly 70% on the demo UI (idle
  frames: about 690 KB to about 200 KB allocated per frame; minor GCs per
  thousand frames: 222 to 71), with identical geometry, damage decisions,
  and widget behavior. Rasterized glyphs are now memoized per raster size
  in the font, so steady-state text measures and draws without FFI calls
  or per-character heap allocation; the glyph walk steps UTF-8 by byte
  offsets and emits quads straight into the draw arena, and damage
  tracking compares the vertex buffer in place instead of copying it every
  frame (a frame that changed nothing allocates nothing to diff). Peak
  live memory is unchanged (the glyph cache adds kilobytes). The demo
  view moved to `examples/DemoCore.hs` behind the unchanged `Demo` entry
  point, and a `chibi-ui-membench` executable runs that exact view
  headlessly over scripted input, reporting allocation per frame and GC
  totals from RTS statistics.
- Added rudimentary damage tracking. Each frame's draw list is diffed
  against the previous one, quad by quad: a frame that changed nothing
  presents without drawing, and a frame that changed a little repaints only
  its rectangles into the retained framebuffer (cleared and redrawn
  scissored). Structural changes, texture uploads, and resizes still paint
  in full. `ChibiUI.Backend` exposes the `Damage` type and `trackFrame` for
  headless hosts.
- Added `textArea`, a multiline field sharing selection, clipboard, undo/redo,
  and context menus with text inputs, with line navigation and a scrollable viewport.
- **Breaking:** views are now `ChibiUI model a`, with a user-defined application
  model and a `MonadState` instance. Replace `useInt`, `useFloat`, `useDouble`,
  `useText`, `useFlag`, and `useState` setter hooks with `get`, `gets`, `put`,
  `modify`, or `modify'`.
- Pass the initial model to `runChibiApp options initialModel view`.
  Model updates persist across frames; deferred menu actions share the model.
- Widget-local editing, focus, selection, and scrolling remain internal.
- Added `treeNode` for collapsible, nested branches with mouse and keyboard
  activation, indented children, and persistent expansion state.
- Restored headless frame tests and the `ChibiUI.Backend` host API, including
  model and tree regression coverage. `runChibiUI` is exported by the backend
  module for use with `newContext initialModel`.

## 0.1.0.0

Initial version: cursor layout, the core widget set (label, button,
textInput, intInput, floatInput, image, table, scrollColumn), state hooks,
and the RGFW/OpenGL backend. Text rasterizes with the bundled RFont from
an embedded TrueType Inter subset; a host can supply its own TTF through
@optFontPath@.
