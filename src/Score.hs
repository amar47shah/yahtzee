module Score ( Combo
             , Name
             , Score
             , combos
             , counters
             , score
             , specials
             ) where

import Roll (Face, Roll, Value, faces, noValue, value, values)
import Utilities (always, count, isIncreasingByOne, windowsOf)

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (nub, sort)
import qualified Data.Map.Strict as M

type Name    = String
type Score   = Maybe Value
type Scoring = Roll -> Value
type Check   = Roll -> Bool
type Table   = M.Map Name Combo

data Combo = Combo { check   :: Check
                   , scoring :: Scoring
                   }

score :: Roll -> Name -> Score
score r n = scoreIf <$> M.lookup n combos <*> pure r

scoreIf :: Combo -> Scoring
scoreIf c = \r -> if check c r then scoring c r else noValue

combos :: Table
combos = countersTable `M.union` specialsTable

counters :: [Name]
counters = M.keys countersTable

specials :: [Name]
specials = M.keys specialsTable

countersTable :: Table
countersTable = M.fromList $ fmap (name &&& counter) faces

name :: Face -> Name
name f = show (value f) ++ "s"

counter :: Face -> Combo
counter f = Combo { check = always, scoring = countAndAddOnly f }

specialsTable :: Table
specialsTable = M.fromList
  [ ("Three of a Kind", Combo { check = hasOfAKind  3, scoring = sumOfValues })
  , ("Four of a Kind" , Combo { check = hasOfAKind  4, scoring = sumOfValues })
  , ("Full House"     , Combo { check = isFullHouse  , scoring = const 25    })
  , ("Small Straight" , Combo { check = hasStraight 4, scoring = const 30    })
  , ("Large Straight" , Combo { check = hasStraight 5, scoring = const 40    })
  , ("Yahtzee"        , Combo { check = hasOfAKind  5, scoring = const 50    })
  , ("Chance"         , Combo { check = always       , scoring = sumOfValues })
  ]

countAndAddOnly :: Face -> Scoring
countAndAddOnly f = (* value f) . length . filter (== f)

sumOfValues :: Scoring
sumOfValues = sum . values

isFullHouse :: Check
isFullHouse = ([2,3] ==) . sort . filter (> 0) . faceCounts

hasStraight :: Int -> Check
hasStraight n = any (isIncreasingByOne . values) . windowsOf n . sort . nub

hasOfAKind :: Int -> Check
hasOfAKind n = any (>= n) . faceCounts

faceCounts :: Roll -> [Int]
faceCounts r = (`count` r) <$> faces
