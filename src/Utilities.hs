module Utilities ( (...)
                 , always
                 , both
                 , count
                 , isIncreasingByOne
                 , replicateA
                 , windowsOf
                 ) where

import Control.Arrow (Arrow, (***))
import Control.Monad (join)
import Data.List (tails)

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

infixr 8 ...

always :: a -> Bool
always = const True

both :: Arrow a => a b c -> a (b, b) (c, c)
both = join (***)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

exactlyLong :: Int -> [a] -> Bool
exactlyLong n = (n ==) . length

isIncreasingByOne :: (Eq a, Num a) => [a] -> Bool
isIncreasingByOne [] = True
isIncreasingByOne xs = all (== 1) $ zipWith (-) (tail xs) xs

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = sequenceA ... replicate

windowsOf :: Int -> [a] -> [[a]]
windowsOf n = filter (exactlyLong n) . map (take n) . tails
