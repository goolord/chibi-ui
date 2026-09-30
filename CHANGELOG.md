# Change log

## Unreleased

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
