module Card where

import Roll (Face, Value, faces)
import Score (Score, Special, specials)
import Utilities ((...))

import Control.Applicative (liftA2)
import qualified Data.Map.Strict as M

data Game = Game { upper :: UpperSection
                 , lower :: LowerSection
                 } deriving Show

type UpperSection = M.Map Face Score
type LowerSection = M.Map Special Score

initial :: Game
initial = Game initialUpper initialLower

initialUpper :: UpperSection
initialUpper = fromKeysWith Nothing faces

initialLower :: LowerSection
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
