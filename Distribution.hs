module Distribution
  ( Distribution
  , new
  , sample
  ) where

import qualified Random

-- A distribution. The entire distribution is 10,000 units wide (so each number
-- represents 1/100 of a percent).
newtype Distribution a
  = Distribution [(Int, a)]
  deriving Functor

new :: [(Int, a)] -> Distribution a
new =
  Distribution

sample
  :: Random.GenIO -- Random generator.
  -> a -- Default element to return, if the distribution was too small.
  -> Distribution a  -- The distribution.
  -> IO a
sample gen x (Distribution xs) = do
  n <- Random.uniformR (0, 10000) gen
  pure (fromMaybe x (sample_ n xs))

sample_ :: Int -> [(Int, a)] -> Maybe a
sample_ n = \case
  [] ->
    Nothing
  (m, x) : xs
    | n < m ->
        Just x
    | otherwise ->
        sample_ (n-m) xs
