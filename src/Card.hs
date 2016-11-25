module Card ( Card
            , bank
            , initial
            , lowerTotal
            , open
            , total
            , upperTotal
            ) where

import Roll (Roll)
import Score (Name, Score, counters, score, specials)
import Utilities (fromKeysWith, sumA)

import Control.Applicative ((<|>))
import Data.Function (on)
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as M

data Card = Card { upper :: Section
                 , lower :: Section
                 } deriving Show

type Section = M.Map Name Score

initial :: Card
initial = (Card `on` fromKeysWith Nothing) counters specials

total :: Card -> Score
total c = (+) <$> upperTotal c <*> lowerTotal c

upperTotal :: Card -> Score
upperTotal = fmap addBonus . sumA . upper
  where addBonus n | n >= 63   = n + 35
                   | otherwise = n

lowerTotal :: Card -> Score
lowerTotal = sumA . lower

open :: Card -> Card
open = upperAndLower $ M.filter isNothing

bank :: Card -> Roll -> Name -> Card
bank c r n = M.adjust (<|> score r n) n `upperAndLower` c

upperAndLower :: (Section -> Section) -> Card -> Card
upperAndLower f = (Card `on` f) <$> upper <*> lower
