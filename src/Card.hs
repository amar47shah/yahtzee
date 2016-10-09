module Card where

import Score (Score, Combo, counters, specials)
import Utilities (fromKeysWith, sumA)

import Data.Function (on)
import qualified Data.Map.Strict as M

data Card = Card { upper :: Section
                 , lower :: Section
                 } deriving Show

type Section = M.Map Combo Score

initial :: Card
initial = (Card `on` fromKeysWith Nothing) counters specials

total :: Card -> Score
total g = (+) <$> upperTotal g <*> lowerTotal g

upperTotal :: Card -> Score
upperTotal = fmap addBonus . sumA . upper
  where addBonus n
         | n >= 63   = n + 35
         | otherwise = n

lowerTotal :: Card -> Score
lowerTotal = sumA . lower
