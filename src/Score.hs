module Score where

import Data.List (nub, sort, tails)

data Die = One
         | Two
         | Three
         | Four
         | Five
         | Six
         deriving (Enum, Eq, Ord, Show)

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

threeOfAKind :: Special
threeOfAKind  = Special { check = hasOfAKind 3, scoring = sumOfValues }

fourOfAKind :: Special
fourOfAKind  = Special { check = hasOfAKind 4, scoring = sumOfValues }

chance :: Special
chance  = Special { check = const True, scoring = sumOfValues }

sumOfValues :: Scoring
sumOfValues = sum . values

yahtzee :: Special
yahtzee  = Special { check = hasOfAKind 5, scoring = const 50 }

hasOfAKind :: Int -> Check
hasOfAKind n = any (>= n) . kindCounts

fullHouse :: Special
fullHouse  = Special { check = isFullHouse, scoring = const 25 }

isFullHouse :: Check
isFullHouse = (`elem` [[2,3], [3,2]]) . filter (/= 0) . kindCounts

kindCounts :: Roll -> [Int]
kindCounts r = (`kindCount` r) <$> dice

kindCount :: Die -> Roll -> Int
kindCount d = length . filter (== d)

smallStraight :: Special
smallStraight  = Special { check = hasStraight 4, scoring = const 30 }

largeStraight :: Special
largeStraight  = Special { check = hasStraight 5, scoring = const 40 }

hasStraight :: Int -> Check
hasStraight n = any isStraight . windowsOfSorted n

isStraight :: Check
isStraight r = case values r of
  vs@(_:vs') -> all (== 1) $ zipWith (-) vs' vs
  _          -> True

windowsOfSorted :: Int -> Roll -> [Roll]
windowsOfSorted n r =
    map (take n) . take m . tails . sort $ u
  where
    u = nub r
    m = 1 - n + length u
