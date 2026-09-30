{-# LANGUAGE OverloadedStrings #-}

-- | A headless memory benchmark that runs the demo UI's exact view through
-- the same per-frame pipeline as the RGFW session (runFrame + damage
-- tracking), against scripted input: idle frames, pointer sweeps, wheel
-- scrolling, a click tour over every widget rect, and typing. Reports
-- allocation per frame, peak live bytes, and GC totals from RTS statistics.
module Main (main) where

import Control.Monad (forM_)
import Data.IORef
import Data.List (sortOn)
import Data.Word (Word64)
import qualified Data.Text as T
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import System.Mem (performMajorGC)
import ChibiUI (get)
import ChibiUI.Backend
import DemoCore (Model (..), demo, demoModel)

-- | The demo window's logical size.
windowW, windowH :: Float
windowW = 520
windowH = 660

-- | One benchmark run: a context, the session-like input reference, the
-- damage snapshot, and damage tallies.
data Bench = Bench
  { benchContext :: Context Model
  , benchInput :: IORef Input
  , benchSnap :: IORef (Maybe FrameSnapshot)
  , benchDamage :: IORef (Int, Int, Int)
  -- ^ full, rects, none
  }

newBench :: IO Bench
newBench = do
  ctx <- newContext demoModel
  inp <- newIORef (emptyInput {inputWindowSize = Size windowW windowH})
  snap <- newIORef Nothing
  dmg <- newIORef (0, 0, 0)
  pure (Bench ctx inp snap dmg)

-- | Run one frame with a tweak, mirroring the session loop: the tweak folds
-- into the persistent input, the view runs, damage is tracked, and one-shot
-- events clear for the next frame. Returns the frame's damage.
stepFrame :: Bench -> (Input -> Input) -> IO Damage
stepFrame b tweak = do
  inp0 <- readIORef (benchInput b)
  let inp = tweak (clearEphemeral inp0) {inputDeltaTime = 0.016}
  writeIORef (benchInput b) inp
  (_, dd) <- runFrame (benchContext b) inp demo
  dmg <- trackFrame (benchSnap b) (inputWindowSize inp) dd
  let bump (f, r, n) = case dmg of
        DamageFull -> (f + 1, r, n)
        DamageRects _ -> (f, r + 1, n)
        DamageNone -> (f, r, n + 1)
  modifyIORef' (benchDamage b) bump
  writeIORef (benchInput b) (clearEphemeral inp)
  pure dmg

-- | A phase's tally of how its frames ended up.
data PhaseStats = PhaseStats
  { psFrames :: !Int
  , psBytes :: !Word64
  , psDamage :: !(Int, Int, Int)
  }

-- | Run @n@ frames with per-frame tweaks numbered @0 .. n - 1@ and measure
-- the allocation delta.
phase :: Bench -> String -> Int -> (Int -> Input -> Input) -> IO PhaseStats
phase b name n tweak = do
  s0 <- getRTSStats
  d0 <- readIORef (benchDamage b)
  forM_ [0 .. n - 1] $ \i -> stepFrame b (tweak i)
  s1 <- getRTSStats
  d1 <- readIORef (benchDamage b)
  let bytes = allocated_bytes s1 - allocated_bytes s0
      ps = PhaseStats n bytes (diff3 d1 d0)
  reportPhase name ps
  pure ps
  where
    diff3 (f, r, e) (f', r', e') = (f - f', r - r', e - e')

-- | Print one phase's line: average bytes per frame and damage mix.
reportPhase :: String -> PhaseStats -> IO ()
reportPhase name ps = do
  let (f, r, e) = psDamage ps
      kbPerFrame = fromIntegral (psBytes ps) / 1024 / max 1 (fromIntegral (psFrames ps)) :: Double
  putStrLn
    ( padRight 10 name
        ++ " frames " ++ padLeft 5 (show (psFrames ps))
        ++ "  alloc/frame " ++ padLeft 9 (kb1 kbPerFrame) ++ " KB"
        ++ "  damage full/rects/none " ++ show f ++ "/" ++ show r ++ "/" ++ show e
    )
  where
    kb1 x = show (fromIntegral (round (x * 10) :: Int) / 10)

padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' ++ s

-- | Pointer position helpers.
at :: Float -> Float -> Input -> Input
at x y inp = inp {inputMousePos = V2 x y}

pressLeft, releaseLeft :: Input -> Input
pressLeft = applyMouseButton MouseLeft True
releaseLeft = applyMouseButton MouseLeft False

typeChar :: Char -> Input -> Input
typeChar c inp = inp {inputChars = inputChars inp ++ [c]}

pressKey :: Key -> Input -> Input
pressKey k inp = inp {inputKeys = inputKeys inp ++ [k]}

main :: IO ()
main = do
  putStrLn ("chibi-ui memory benchmark: demo view, " ++ show (windowW, windowH))
  b <- newBench
  -- Warm up: rasterize glyphs, grow the draw arena to the demo's steady
  -- size, and settle the damage snapshot.
  _ <- phase b "warmup" 80 (const id)
  -- Steady state: nothing happens. This is what an idle window costs per
  -- wakeup, and where allocation matters most.
  _ <- phase b "idle" 400 (const id)
  -- Pointer motion sweeping down the window: hover highlights and cursor
  -- changes keep every frame a small-damage frame.
  _ <- phase b "motion" 240 (\i -> at 260 (8 + fromIntegral (i `mod` 158) * 4))
  -- Wheel scrolling over the scroll column until it clamps, then keeps
  -- wheeling while pinned at the bottom.
  _ <- phase b "scroll" 200 (\_ -> (at 260 (windowH - 30)) . (\i -> i {inputScroll = V2 0 1}))
  -- A click tour: press, release, and one typed character at the centre of
  -- every widget rect, in layout order. Exercises buttons, fields, the
  -- table, the tree, and the context-menu plumbing.
  rects <- frameRects (benchContext b)
  let centres =
        [ (rectX r + rectW r / 2, rectY r + rectH r / 2)
        | (_, r) <- sortOn (\(_, r) -> (rectY r, rectX r)) rects
        ]
      tour = take 48 centres
  s0 <- getRTSStats
  d0 <- readIORef (benchDamage b)
  forM_ tour $ \(x, y) -> do
    _ <- stepFrame b (at x y . pressLeft)
    _ <- stepFrame b (at x y . releaseLeft)
    _ <- stepFrame b (typeChar 'x')
    pure ()
  s1 <- getRTSStats
  d1 <- readIORef (benchDamage b)
  reportPhase
    "click-tour"
    PhaseStats
      { psFrames = length tour * 3
      , psBytes = allocated_bytes s1 - allocated_bytes s0
      , psDamage = d1 `minus` d0
      }
  -- Typing with focus moving by Tab every ten frames.
  _ <- phase b "typing" 120 $ \i ->
    let tab = if i `mod` 10 == 0 then pressKey KeyTab else id
        chars = typeChar (T.index "the quick brown fox" (i `mod` 19))
     in chars . tab
  -- Steady state again, after interaction.
  _ <- phase b "idle2" 200 (const id)
  -- Totals.
  performMajorGC
  s <- getRTSStats
  (f, r, e) <- readIORef (benchDamage b)
  model <- runChibiUI (benchContext b) get
  putStrLn ""
  putStrLn ("model survived: name=" ++ show (name model) ++ " count=" ++ show (count model))
  putStrLn
    ( "totals: max_live "
        ++ mb (max_live_bytes s)
        ++ " MB, live_now "
        ++ mb (gcdetails_live_bytes (gc s))
        ++ " MB, minor_gcs "
        ++ show (gcs s - major_gcs s)
        ++ ", major_gcs "
        ++ show (major_gcs s)
        ++ ", gc_cpu "
        ++ show (fromIntegral (gc_cpu_ns s) / 1e9 :: Double)
        ++ "s, damage full/rects/none "
        ++ show f ++ "/" ++ show r ++ "/" ++ show e
    )
  where
    minus (f, r, e) (f', r', e') = (f - f', r - r', e - e')
    mb w = show (fromIntegral w / 1048576 :: Double)
