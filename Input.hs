module Input
  ( Input(..)
  , Key(..)
  , makeInput
  ) where

import Coerce (unsafeCoerce)
import FRP

import qualified UI.NCurses as Curses

data Input
  = Time Int
  | Key Key
  deriving Eq

data Key
  = KeyChar Char
  | KeyTab
  deriving Eq

makeInput :: Curses.Window -> MomentIO (Event Input)
makeInput window = do
  eTime :: Event Int <-
    tickEvery 100000

  eCurses :: Event Curses.Event <-
    curses window

  let eKey :: Event Key
      eKey =
        filterJust
          ((\case
            Curses.EventCharacter c ->
              Just (KeyChar c)
            Curses.EventSpecialKey _ ->
              Nothing
            Curses.EventMouse _ _ ->
              Nothing
            Curses.EventResized ->
              Nothing
            Curses.EventUnknown _ ->
              Nothing)
            <$> eCurses)

  pure
    (foldr (unionWith const) never
      [ Time <$> eTime
      , Key <$> eKey
      ])

tickEvery :: Int -> MomentIO (Event Int)
tickEvery micro = do
  (eTick :: Event (), fireTick :: () -> IO ()) <-
    newEvent

  eTime :: Event Int <-
    accumE 0 ((+1) <$ eTick)

  _ <- liftIO . forkIO . forever $ do
    fireTick ()
    threadDelay micro

  pure eTime

curses :: Curses.Window -> MomentIO (Event Curses.Event)
curses window = do
  (eEvent :: Event Curses.Event, fireEvent :: Curses.Event -> IO ()) <-
    newEvent

  _ <- liftIO . forkIO . forever $ do
    Just evt <-
      unsafeCoerce (Curses.getEvent window Nothing)
    fireEvent evt

  pure eEvent
