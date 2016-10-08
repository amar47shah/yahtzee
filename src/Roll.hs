module Roll ( Die
            , Roll
            , Value
            , dice
            , fromValue
            , fromValues
            , noValue
            , value
            , values
            ) where

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
