module Card where

import Roll (Face, Value, faces)
import Score (Score, Special, specials)
import Utilities ((...))

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
