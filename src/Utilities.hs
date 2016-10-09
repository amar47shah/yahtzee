module Utilities ( (...)
                 , always
                 , both
                 , count
                 , fromKeysWith
                 , isIncreasingByOne
                 , replicateA
                 , sumA
                 , windowsOf
                 ) where

import Control.Applicative (liftA2)
import Control.Arrow (Arrow, (***))
import Control.Monad (join)
import Data.List (tails)
import qualified Data.Map.Strict as M

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

fromKeysWith :: Ord a => b -> [a] -> M.Map a b
fromKeysWith = M.fromList ... fmap . flip (,)

isIncreasingByOne :: (Eq a, Num a) => [a] -> Bool
isIncreasingByOne [] = True
isIncreasingByOne xs = all (== 1) $ zipWith (-) (tail xs) xs

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = sequenceA ... replicate

sumA :: (Num a, Foldable t, Applicative f) => t (f a) -> f a
sumA = foldr (liftA2 (+)) $ pure 0

windowsOf :: Int -> [a] -> [[a]]
windowsOf n = filter (exactlyLong n) . map (take n) . tails
