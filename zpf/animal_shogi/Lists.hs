module Lists where

import Control.Applicative

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

none :: (a -> Bool) -> [a] -> Bool
none pred = not . any pred

filterAnd :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterAnd pred1 pred2 = filter (liftA2 (&&) pred1 pred2)

{- Seems to be quicker -}
filterOr' pred1 pred2 = filter pred2 . filter pred1 

filterOr :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterOr pred1 pred2 = filter (liftA2 (||) pred1 pred2)
