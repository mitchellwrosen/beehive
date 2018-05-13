module Render
  ( Render
  , render
  , draw
  ) where

import Reader

import qualified UI.NCurses as Curses

type Render a
  = ReaderT (Int, Int) Curses.Update a

render :: Render () -> Curses.Window -> Curses.Curses ()
render action window =
  Curses.updateWindow window $ do
    (wr, wc) <- Curses.windowSize
    runReaderT action (fromIntegral wr, fromIntegral wc)

draw :: Int -> Int -> Text -> Render ()
draw r c s =
  lift $ do
    Curses.moveCursor (fromIntegral r) (fromIntegral c)
    Curses.drawText s
