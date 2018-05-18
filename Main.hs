import Distribution (Distribution)
import Input
import Render (Display(Inverted, Raw), Render)

import qualified Distribution
import qualified Render

import Coerce (unsafeCoerce)
import FRP
import List.Partial ((!!))
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

    network :: EventNetwork <- do
      liftIO . compile $ do
        gen :: Random.GenIO <-
          liftIO Random.createSystemRandom

        eInput :: Event Input <-
          makeInput window

        -- The time-varying scene to render.
        bScene :: Behavior (Render ()) <-
          moment gen eInput

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

data Action
  = Writhe
  deriving Eq

showAction :: Action -> Text
showAction = \case
  Writhe ->
    "writhe"

--------------------------------------------------------------------------------
-- Core game logic
--------------------------------------------------------------------------------

moment
  :: Random.GenIO
  -> Event Input
  -> MomentIO (Behavior (Render ()))
moment gen eInput = mdo
  -- The elapsed time.
  let eTime :: Event Int
      eTime =
        filterJust
          ((\case
            Time n ->
              Just n
            _ ->
              Nothing)
          <$> eInput)
  bTime :: Behavior Int <-
    stepper 0 eTime

  -- Enter presses.
  let eKeyEnter :: Event ()
      eKeyEnter =
        filterJust
          ((\case
            Key (KeyChar '\n') ->
              Just ()
            _ ->
              Nothing)
            <$> eInput)

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

  -- The available actions.
  let bAvailableActions :: Behavior [Action]
      bAvailableActions =
        (\case
          StageEgg ->
            []
          StageLarva ->
            [Writhe])
        <$> bStage

  -- The selected action. When there are no available actions, the value is
  -- irrelevant (but should be 0).
  bSelectedAction :: Behavior Int <-
    accumB 0 ((+1) <$ filterE (== Key (KeyChar '\t')) eInput)

  -- The actions performed.
  let eAction :: Event Action
      eAction =
        filterJust
          ((\actions i ->
            case actions of
              [] ->
                Nothing
              _ ->
                Just (actions !! i))
            <$> bAvailableActions
            <*> bSelectedAction
            <@  eKeyEnter)

  -- The distribution of ambient text that might be logged at every tick.
  let bAmbience :: Behavior (Distribution Text)
      bAmbience =
        (\stage ->
          case stage of
            StageEgg ->
              Distribution.new
                [ (200, "Cchh.")
                , (200, "Kh.")
                ]
            StageLarva ->
              Distribution.new
                [ (200, "Mmmm. Jelly.")
                ])
        <$> bStage

  -- The event log. Grows and grows and grows with every message meant to be
  -- output to the console.

  eAmbientLog :: Event (Maybe Text) <-
    mapEventIO
      (\d -> Distribution.sample gen Nothing (Just <$> d))
      (bAmbience <@ eTime)

  eLog :: Event [Text] <- do
    accumE []
      (unions
        [ maybe id (:) <$> eAmbientLog
        , (:)
          . (\case
              Writhe ->
                "Wiggle, wiggle.")
          <$> eAction
        , ("Phhfffffllp." :) <$ eBecomeLarva
          -- For debugging purposes: log the time
        -- , (\n ss -> pack (show n) : ss) <$> eTime
        ])
  bLog :: Behavior [Text] <-
    stepper [] eLog

  pure
    (render
      <$> bTime
      <*> bLog
      <*> bAvailableActions
      <*> bSelectedAction)

-- The main rendering function.
render :: Int -> [Text] -> [Action] -> Int -> Render ()
render time ss actions selectedAction = do
  (wr, wc) <- ask

  Render.drawAt 0 0 (Raw (renderTime time))

  -- Render the log bottom-up, starting at the second-to-last line and ending
  -- on the first line. Each line is padded with whitespace on the right that
  -- extends to the end of the screen.

  for_ (zip [0..] (take (wr-2) ss)) $ \(i, s) ->
    Render.drawAt (wr - i - 2) 0 (Raw (Text.justifyLeft (wc-1) ' ' s))

  -- Render the actions at the bottom of the screen. Highlight the selected
  -- action, and at the end, render whitespace to the end of the screen. This
  -- is necessary to clear any artifacts from a previous, longer list of
  -- actions.

  Render.move (wr - 1) 0
  for_ (zip [0..] actions) $ \(i, s) -> do
    -- We won't execute (`mod` 0) here, which would throw divide-by-0, because
    -- if length actions == 0 then we never reach this code
    if i == selectedAction `mod` length actions
      then Render.draw (Inverted (showAction s))
      else Render.draw (Raw (showAction s))
    Render.draw (Raw " ")
  Render.clearLine

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
