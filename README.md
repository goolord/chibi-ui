# chibi-ui

`chibi-ui` is a tiny immediate-mode GUI library for Haskell.
- cursor layout engine (`sameLine` + `row` | `column`, othwerwise monadic sequencing means "newline")
- a small set of widgets.
- one backend: RGFW+OpenGL.
- minimal dependencies
- embedded 22KB font

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import ChibiUI
import ChibiUI.Backend.Rgfw (defaultRgfwOptions, runChibiApp)

main :: IO ()
main = runChibiApp defaultRgfwOptions (Model 0) counter

data Model = Model { count :: Int }

counter :: ChibiUI Model ()
counter = do
  up <- button "+"
  when up (modify (\m -> m { count = count m + 1 }))
  sameLine
  n <- gets count
  label (T.pack (show n))
  sameLine
  down <- button "-"
  when down (modify (\m -> m { count = count m - 1 }))
```

Record fields are lenses with `ChibiUI.Labels` and a `Generic` model, so
`edit` and `mapModel` take them by label, and microlens-mtl's operators
update them:

```haskell
{-# LANGUAGE OverloadedLabels #-}

import GHC.Generics (Generic)
import ChibiUI.Labels ()
import Lens.Micro.Mtl (use, (%=))

data Model = Model { name :: Text, count :: Int } deriving Generic

view :: ChibiUI Model ()
view = do
  _ <- edit #name textInput
  up <- button "+"
  when up (#count %= (+ 1))
  label . T.pack . show =<< use #count
```

## Build

```sh
cabal run chibi-ui-demo   # widget tour
cabal test chibi-ui-test  # headless frame tests
```

## Credits

- [RGFW](https://github.com/ColleagueRiley/RGFW) and
  [RFont](https://github.com/ColleagueRiley/RFont), both MIT, vendored in
  `cbits/`.
- The embedded `data/inter.ttf` is an Inter subset; Inter is licensed under the SIL Open Font License.

## License

MIT
