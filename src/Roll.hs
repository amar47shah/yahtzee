module Roll ( Die
            , Roll
            , Value
            , dice
            , fromValue
            , fromValues
            , noValue
            , randomFive
            , value
            , values
            ) where

import Utilities ((...), both, replicateA)

import Control.Arrow (first)
import Control.Monad (join)
import System.Random (Random, random, randomIO, randomR)

data Die = One
         | Two
         | Three
         | Four
         | Five
         | Six
         deriving (Enum, Bounded, Eq, Ord, Show)

dice :: [Die]
dice = enumFrom One

type Value = Int

value :: Die -> Value
value = succ . fromEnum

fromValue :: Value -> Die
fromValue = toEnum . pred

noValue :: Value
noValue = 0

type Roll = [Die]

fromValues :: [Value] -> Roll
fromValues = fmap fromValue

values :: Roll -> [Value]
values = fmap value

instance Random Die where
  random = randomR (minBound, maxBound)
  randomR = first toEnum ... randomR . both fromEnum

randomDie :: IO Die
randomDie = randomIO

randomRoll :: Int -> IO Roll
randomRoll n = replicateA n randomDie

randomFive :: IO Roll
randomFive = randomRoll 5
