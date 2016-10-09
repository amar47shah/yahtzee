module Score ( Score
             , Special
             , specials
             ) where

import Roll (Face, Roll, Value, faces, noValue, value, values)
import Utilities (count, isIncreasingByOne, windowsOf)

import Data.Function (on)
import Data.List (nub, sort)

type Score = Maybe Value
type Scoring = Roll -> Value

countAndAddOnly :: Face -> Scoring
countAndAddOnly d = (* value d) . length . filter (== d)

type Check   = Roll -> Bool
data Special = Special { name    :: String
                       , check   :: Check
                       , scoring :: Scoring
                       }

instance Eq Special where
  (==) = (==) `on` name

instance Ord Special where
  compare = compare `on` name

instance Show Special where
  show = show . name

specials :: [Special]
specials =
  [ Special { name = "3 of a Kind"   , check = hasOfAKind  3, scoring = sumOfValues }
  , Special { name = "4 of a Kind"   , check = hasOfAKind  4, scoring = sumOfValues }
  , Special { name = "Full House"    , check = isFullHouse  , scoring = const 25    }
  , Special { name = "Small Straight", check = hasStraight 4, scoring = const 30    }
  , Special { name = "Large Straight", check = hasStraight 5, scoring = const 40    }
  , Special { name = "Yahtzee"       , check = hasOfAKind  5, scoring = const 50    }
  , Special { name = "Chance"        , check = const True   , scoring = sumOfValues }
  ]

scoreIf :: Special -> Scoring
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
