module Main where

import Card (Card, initial, bank, total)
import Roll (reroll, roll)
import Data.Maybe (isJust)

main :: IO Card
main = untilM (isJust . total) turn initial

turn :: Card -> IO Card
turn c =
  roll
    >>= (`reroll` [])
      >>= (`reroll` [])
        >>= \r -> print r >> pure r
          >>= \r -> bank c r <$> getLine

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p k x
 | p x       = pure x
 | otherwise = k x >>= untilM p k
