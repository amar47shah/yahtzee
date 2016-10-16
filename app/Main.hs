module Main where

import Card (Card, initial, bank, total)
import Roll (Roll, reroll, roll)
import Data.Maybe (isJust)
import System.Random (randomRIO)

main :: IO Card
main = untilM (isJust . total) turn initial

turn :: Card -> IO Card
turn c = print c
      *> roll
     >>= printAndReturn
     >>= randomReroll
     >>= printAndReturn
     >>= randomReroll
     >>= printAndReturn
     >>= (<$> getLine) . bank c

randomReroll :: Roll -> IO Roll
randomReroll r = (`splitAt` r) <$> randomRIO (0, length r) >>= uncurry reroll

printAndReturn :: Show a => a -> IO a
printAndReturn x = print x *> pure x

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p k x
 | p x       = pure x
 | otherwise = k x >>= untilM p k
