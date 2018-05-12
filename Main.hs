import Coerce (unsafeCoerce)
import FRP
import Reader
import State
import Text (pack)

import qualified UI.NCurses as Curses

main :: IO ()
main = do
  Curses.runCurses $ do
    Curses.setEcho False
    _ <- Curses.setCursorMode Curses.CursorInvisible

    window :: Curses.Window <-
      Curses.defaultWindow

    (tickAddHandler, fireTick) :: (AddHandler (), Handler ()) <-
      liftIO newAddHandler

    _ <- liftIO . forkIO . forever $ do
      fireTick ()
      threadDelay 1000000

    network :: EventNetwork <- do
      liftIO . compile $ do
        eTime :: Event Int <- do
          eTick :: Event () <-
            fromAddHandler tickAddHandler
          accumE 0 ((+1) <$ eTick)

        -- The time-varying scene to render.
        bScene :: Behavior (Render ()) <-
          moment eTime

        let doRender :: Render () -> IO ()
            doRender action =
              unCurses $ do
                render action window
                Curses.render

        -- Render the very first scene.
        scene0 :: Render () <-
          valueB bScene
        liftIO (doRender scene0)

        -- Render the scene every time it changes.
        eScene :: Event (Future (Render ())) <-
          changes bScene
        reactimate' ((fmap.fmap) doRender eScene)

    liftIO (actuate network)

    liftIO (forever (threadDelay maxBound))

moment
  :: Event Int -- Time
  -> MomentIO (Behavior (Render ()))
moment eTime = mdo
  eLog :: Event [Text] <-
    accumE [] ((\n ss -> "Time = " <> pack (show n) : ss) <$> eTime)
  bLog :: Behavior [Text] <-
    stepper [] eLog

  let bRender :: Behavior (Render ())
      bRender =
        (\ss -> do
          (wr, wc) <- ask
          for (zip [0..] (take wr ss)) $ \(i, s) -> do
            move (wr - i - 1) 0
            draw s
          pure ())
        <$> bLog

  pure bRender

type Render a
  = ReaderT (Int, Int) Curses.Update a

render :: Render () -> Curses.Window -> Curses.Curses ()
render action window =
  Curses.updateWindow window $ do
    (wr, wc) <- Curses.windowSize
    runReaderT action (fromIntegral wr, fromIntegral wc)

draw :: Text -> Render ()
draw =
  lift . Curses.drawText

move :: Int -> Int -> Render ()
move r c =
  lift (Curses.moveCursor (fromIntegral r) (fromIntegral c))

unCurses :: Curses.Curses a -> IO a
unCurses =
  unsafeCoerce
