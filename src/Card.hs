module Card where

import Score (Score, Combo, counters, specials)
import Utilities ((...))

import Control.Applicative (liftA2)
import qualified Data.Map.Strict as M

data Game = Game { upper :: Section
                 , lower :: Section
                 } deriving Show

type Section = M.Map Combo Score

initial :: Game
initial = Game initialUpper initialLower

initialUpper :: Section
initialUpper = fromKeysWith Nothing counters

initialLower :: Section
initialLower = fromKeysWith Nothing specials

fromKeysWith :: Ord a => b -> [a] -> M.Map a b
fromKeysWith = M.fromList ... fmap . flip (,)

total :: Game -> Score
total g = (+) <$> upperTotal g <*> lowerTotal g

upperTotal :: Game -> Score
upperTotal = fmap addBonus . sumA . upper
  where addBonus n
         | n >= 63   = n + 35
         | otherwise = n

lowerTotal :: Game -> Score
lowerTotal = sumA . lower

sumA :: (Num a, Foldable t, Applicative f) => t (f a) -> f a
sumA = foldr (liftA2 (+)) $ pure 0
