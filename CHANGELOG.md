# Change log

## Unreleased

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
