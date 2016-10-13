module Roll ( Face
            , Roll
            , Value
            , faces
            , fromValue
            , fromValues
            , noValue
            , reroll
            , roll
            , value
            , values
            ) where

import Utilities ((...), both, replicateA)

import Control.Arrow (first)
import Control.Monad (join)
import System.Random (Random, random, randomIO, randomR)

data Face = One
          | Two
          | Three
          | Four
          | Five
          | Six
          deriving (Enum, Bounded, Eq, Ord, Show)

faces :: [Face]
faces = enumFrom One

type Value = Int

value :: Face -> Value
value = succ . fromEnum

fromValue :: Value -> Face
fromValue = toEnum . pred

noValue :: Value
noValue = 0

type Roll = [Face]

fromValues :: [Value] -> Roll
fromValues = fmap fromValue

values :: Roll -> [Value]
values = fmap value

instance Random Face where
  random = randomR (minBound, maxBound)
  randomR = first toEnum ... randomR . both fromEnum

randomFace :: IO Face
randomFace = randomIO

randomRoll :: Int -> IO Roll
randomRoll n = replicateA n randomFace

roll :: IO Roll
roll = randomRoll 5

reroll :: Roll -> Roll -> IO Roll
reroll r = fmap (r ++) . randomRoll . length
