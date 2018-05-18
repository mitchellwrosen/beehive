module Render
  ( Render
  , Display(..)
  , render
  , draw
  , drawAt
  , move
  , clearLine
  ) where

import Coerce (unsafeCoerce)
import IO (unsafePerformIO)
import Reader

import qualified Text
import qualified UI.NCurses as Curses

--------------------------------------------------------------------------------
-- Render types
--------------------------------------------------------------------------------

type Render a
  = ReaderT (Int, Int) Curses.Update a

data Display
  = Raw Text
  | Inverted Text

--------------------------------------------------------------------------------
-- Global variables
--------------------------------------------------------------------------------

invertedColorID :: Curses.ColorID
invertedColorID =
  unsafePerformIO
    (unsafeCoerce (Curses.newColorID Curses.ColorBlack Curses.ColorWhite 1))
{-# NOINLINE invertedColorID #-}

--------------------------------------------------------------------------------
-- Render API
--------------------------------------------------------------------------------

render :: Render () -> Curses.Window -> Curses.Curses ()
render action window =
  Curses.updateWindow window $ do
    (wr, wc) <- Curses.windowSize
    runReaderT action (fromIntegral wr, fromIntegral wc)

draw :: Display -> Render ()
draw = \case
  Raw s ->
    lift (Curses.drawText s)
  Inverted s ->
    lift $ do
      Curses.setColor invertedColorID
      Curses.drawText s
      Curses.setColor Curses.defaultColorID

move :: Int -> Int -> Render ()
move r c =
  lift (Curses.moveCursor (fromIntegral r) (fromIntegral c))

drawAt :: Int -> Int -> Display -> Render ()
drawAt r c s = do
  move r c
  draw s

-- | Clear the rest of the line with whitespace.
clearLine :: Render ()
clearLine = do
  (_, wc) <- ask
  (_, cc) <- lift Curses.cursorPosition
  lift (Curses.drawText (Text.replicate (wc - fromIntegral cc - 1) " "))
