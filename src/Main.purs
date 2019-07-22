module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Array (range, null, (..), filter, concat, concatMap)
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)
import Data.Foldable (product)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

isEven :: Int -> Boolean
isEven value = (value `mod` 2) == 0

evenInts2 :: Int -> Int
evenInts2 val =
  if val == 3
    then 1
    else
      if val == 2
        then 3
        else 5


evenInts :: Array Int -> Int
evenInts arr =
  if null arr
    then 0
    else
      if ((unsafePartial head arr) `mod` 2) == 0
        then (unsafePartial head arr) + evenInts (unsafePartial tail arr)
        else evenInts (unsafePartial tail arr)

incArr :: Array Int -> Array Int
--incArr arr = map (\n -> n + 1) arr
--incArr arr =  (\n -> n + 1) `map` arr
incArr arr =  (\n -> n + 1) <$> arr

infix 8 filter as <$?>

pairs :: Int -> Array Int
pairs n = concatMap (\i -> 1 .. n) (1 .. n)
pairs' n = concatMap (\i -> map(\j -> [i, j]) (1 .. n)) (1 .. n)
pairs'' n = concatMap (\i -> map(\j -> [i, j]) (i .. n)) (1 .. n)
factors n = filter (\pair -> product pair == n) (pairs'' n)

factors' :: Int -> Array (Array Int)
factors' n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  --pure [i, j]
  [[i, j]]

main :: Effect Unit
main = do
  log (show (fact 2))
  log (show (fib 2))
  log (show (fib 3))
  log (show (fib 4))
  log (show (length [1,2,4]))
  log (show (isEven 2))
  log (show (isEven 5))
  log (show ((unsafePartial head [4,5,9]) `mod` 2))
  log (show (unsafePartial head [4,5,9]))
  log (show (unsafePartial tail [9]))
  log (show (evenInts2 3))
  log (show (evenInts2 2))
  log (show (evenInts2 1))
  log (show (evenInts2 5))
  log (show (evenInts [5, 9, 1, 2, 4, 6, 7]))
  log (show (incArr [5, 9, 1, 2, 4, 6, 7]))
  log (show (show <$> [1,2]))
  log (show ((<$>) show [1,2]))
  log (show (show <$> (range 1 3)))
  log (show (show <$> (1 `range` 3)))
  log (show (show <$> (1 `range` 3)))
  log (show (show <$> (1 .. 3))) -- alias for range
  log (show (show <$> 1 .. 3)) -- .. higher percedence in library no () needed
  log (show (filter (\n -> n `mod` 2 == 0) (1 .. 10)))
  log (show (map (\n -> n * n) (1 .. 10)))
  log (show ((<$>) (\n -> n * n) (1 .. 10)))
  log (show ( (\n -> n * n) <$> (1 .. 10)))
  log ("filter start")
  log (show (filter (\n -> n > -1) (-10 .. 10)))
  log (show ((\n -> n > -1) `filter` (-10 .. 10)))
  log (show ((\n -> n > -1) <$?> (-10 .. 10)))
  log (show ((<$?>) (\n -> n > -1)  (-10 .. 10)))
  log (show ((\n -> n > -1) <$?> (-10 .. 10)))
  log ("flatten start")
  log (show (concat [[1,2],[4,5,6], [7]]))
  log (show (concatMap (\n -> [n, n*n]) (1 .. 5)))
  log ("arr comprehensions")
  log (show (pairs 3))
  log (show (pairs' 3))
  log (show (pairs'' 3))
  log (show (factors 3))
  log (show (factors 10))
  log (show (factors' 10))
