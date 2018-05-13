module Render
  ( Render
  , Display(..)
  , render
  , draw
  ) where

import Reader

import qualified UI.NCurses as Curses

type Render a
  = ReaderT (Int, Int) Curses.Update a

data Display
  = Raw Text
  | Inverted Text

render :: Render () -> Curses.Window -> Curses.Curses ()
render action window =
  Curses.updateWindow window $ do
    (wr, wc) <- Curses.windowSize
    runReaderT action (fromIntegral wr, fromIntegral wc)

draw :: Int -> Int -> Display -> Render ()
draw r c s =
  lift $ do
    Curses.moveCursor (fromIntegral r) (fromIntegral c)
    case s of
      Raw s' ->
        Curses.drawText s'
      Inverted s' ->
        -- TODO: Invert
        Curses.drawText s'
