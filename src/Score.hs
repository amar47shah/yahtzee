module Score where

import Roll (Die, Roll, Value, dice, noValue, value, values)
import Data.List (nub, sort, tails)

type Scoring = Roll -> Value

countAndAddOnly :: Die -> Scoring
countAndAddOnly d = (* value d) . length . filter (== d)

type Check   = Roll -> Bool
data Special = Special { check   :: Check
                       , scoring :: Scoring
                       }

scoreIf :: Special -> Scoring
scoreIf s = \r ->
  case check s r of
    True -> scoring s r
    _    -> noValue

threeOfAKind, fourOfAKind, fullHouse, smallStraight, largeStraight, yahtzee, chance :: Special
threeOfAKind  = Special { check = hasOfAKind  3, scoring = sumOfValues }
fourOfAKind   = Special { check = hasOfAKind  4, scoring = sumOfValues }
fullHouse     = Special { check = isFullHouse  , scoring = const 25    }
smallStraight = Special { check = hasStraight 4, scoring = const 30    }
largeStraight = Special { check = hasStraight 5, scoring = const 40    }
yahtzee       = Special { check = hasOfAKind  5, scoring = const 50    }
chance        = Special { check = const True   , scoring = sumOfValues }

sumOfValues :: Scoring
sumOfValues = sum . values

hasOfAKind :: Int -> Check
hasOfAKind n = any (>= n) . kindCounts

isFullHouse :: Check
isFullHouse = ([2,3] ==) . sort . filter (> 0) . kindCounts

kindCounts :: Roll -> [Int]
kindCounts r = (`count` r) <$> dice

hasStraight :: Int -> Check
hasStraight n = any (isIncreasingByOne . values) . windowsOf n . sort . nub

-- Utilities

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

isIncreasingByOne :: (Eq a, Num a) => [a] -> Bool
isIncreasingByOne [] = True
isIncreasingByOne xs = all (== 1) $ zipWith (-) (tail xs) xs

windowsOf :: Int -> [a] -> [[a]]
windowsOf n = filter (exactlyLong n) . map (take n) . tails
  where exactlyLong n = (n ==) . length
