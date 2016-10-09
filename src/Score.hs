module Score ( Score
             , Combo
             , counters
             , specials
             ) where

import Roll (Face, Roll, Value, faces, noValue, value, values)
import Utilities (always, count, isIncreasingByOne, windowsOf)

import Data.Function (on)
import Data.List (nub, sort)

type Score = Maybe Value
type Scoring = Roll -> Value

type Check   = Roll -> Bool
data Combo = Combo { name    :: String
                   , check   :: Check
                   , scoring :: Scoring
                   }

instance Eq Combo where
  (==) = (==) `on` name

instance Ord Combo where
  compare = compare `on` name

instance Show Combo where
  show = show . name

counters :: [Combo]
counters = counter <$> faces
      where
  counter f = Combo { name    = show (value f) ++ "s"
                    , check   = always
                    , scoring = countAndAddOnly f
                    }

countAndAddOnly :: Face -> Scoring
countAndAddOnly d = (* value d) . length . filter (== d)

specials :: [Combo]
specials =
  [ Combo { name = "Three of a Kind", check = hasOfAKind  3, scoring = sumOfValues }
  , Combo { name = "Four of a Kind" , check = hasOfAKind  4, scoring = sumOfValues }
  , Combo { name = "Full House"     , check = isFullHouse  , scoring = const 25    }
  , Combo { name = "Small Straight" , check = hasStraight 4, scoring = const 30    }
  , Combo { name = "Large Straight" , check = hasStraight 5, scoring = const 40    }
  , Combo { name = "Yahtzee"        , check = hasOfAKind  5, scoring = const 50    }
  , Combo { name = "Chance"         , check = always       , scoring = sumOfValues }
  ]

scoreIf :: Combo -> Scoring
scoreIf s = \r -> if check s r then scoring s r else noValue

sumOfValues :: Scoring
sumOfValues = sum . values

hasOfAKind :: Int -> Check
hasOfAKind n = any (>= n) . faceCounts

isFullHouse :: Check
isFullHouse = ([2,3] ==) . sort . filter (> 0) . faceCounts

faceCounts :: Roll -> [Int]
faceCounts r = (`count` r) <$> faces

hasStraight :: Int -> Check
hasStraight n = any (isIncreasingByOne . values) . windowsOf n . sort . nub
