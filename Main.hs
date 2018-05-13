import Distribution (Distribution)
import Render (Render)

import qualified Distribution
import qualified Render

import Coerce (unsafeCoerce)
import FRP
import Reader
import Text (pack)

import qualified Random
import qualified Text

import qualified UI.NCurses as Curses

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

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
      threadDelay 100000

    network :: EventNetwork <- do
      liftIO . compile $ do
        eTime :: Event Int <- do
          eTick :: Event () <-
            fromAddHandler tickAddHandler
          accumE 0 ((+1) <$ eTick)

        gen :: Random.GenIO <-
          liftIO Random.createSystemRandom

        -- The time-varying scene to render.
        bScene :: Behavior (Render ()) <-
          moment gen eTime

        let doRender :: Render () -> IO ()
            doRender action =
              unCurses $ do
                Render.render action window
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

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

data Stage
  = StageEgg
  | StageLarva
  deriving Eq

--------------------------------------------------------------------------------
-- Core game logic
--------------------------------------------------------------------------------

moment
  :: Random.GenIO
  -> Event Int -- Time
  -> MomentIO (Behavior (Render ()))
moment gen eTime = mdo
  bTime :: Behavior Int <-
    stepper 0 eTime

  -- The queen becomes a larva. Fires only once.
  eBecomeLarva :: Event () <- do
    n <- liftIO (Random.uniformR (100, 200) gen)
    pure (() <$ filterE (== n) eTime)

  -- The stage of the queen.
  bStage :: Behavior Stage <-
    switchB (pure StageEgg)
      (leftmost
        [ pure StageLarva <$ eBecomeLarva
        ])

  -- The distribution of ambient text that might be logged at every tick.
  let bAmbience :: Behavior (Distribution Text)
      bAmbience =
        (\stage ->
          case stage of
            StageEgg ->
              Distribution.new
                [ (200, "Wiggle.")
                ]
            StageLarva ->
              Distribution.new
                [ (100, "Wiggle wiggle.")
                , (100, "Mmmm. Jelly.")
                ])
        <$> bStage

  -- The event log. Grows and grows and grows with every message meant to be
  -- output to the console.
  eLog :: Event [Text] <- do
    eAmbientLog :: Event (Maybe Text) <-
      mapEventIO
        (\d -> Distribution.sample gen Nothing (Just <$> d))
        (bAmbience <@ eTime)

    accumE []
      (unions
        [ maybe id (:) <$> eAmbientLog
        , ("Phhfffffllp." :) <$ eBecomeLarva
          -- For debugging purposes: log the time
        -- , (\n ss -> pack (show n) : ss) <$> eTime
        ])
  bLog :: Behavior [Text] <-
    stepper [] eLog

  pure (render <$> bTime <*> bLog)

-- The main rendering function.
render :: Int -> [Text] -> Render ()
render time ss = do
  (wr, wc) <- ask

  Render.draw 0 0 (renderTime time)

  for_ (zip [0..] (take (wr-1) ss)) $ \(i, s) ->
    Render.draw (wr - i - 1) 0 (Text.justifyLeft (wc-1) ' ' s)

--------------------------------------------------------------------------------
-- Miscellaneous functions
--------------------------------------------------------------------------------

renderTime :: Int -> Text
renderTime time =
  "Day " <> pack (show day) <> ", " <>
    if hour < 13
      then pack (show hour) <> ":00 am "
      else pack (show (hour - 12)) <> ":00 pm "
 where
  (day, hour) = timeDay time

timeDay :: Int -> (Int, Int)
timeDay time =
  (day + 1, hour)
 where
  (day, delta) = time `divMod` 100
  hour = delta * 24 `div` 100

unCurses :: Curses.Curses a -> IO a
unCurses =
  unsafeCoerce

--------------------------------------------------------------------------------
-- reactive-banana extras
--------------------------------------------------------------------------------

leftmost :: [Event a] -> Event a
leftmost =
  foldr (unionWith const) never
