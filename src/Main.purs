module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Array (range, null, (..), filter, concat, concatMap, length, snoc, cons)
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)
import Data.Foldable (product, foldl, foldr)
import Control.MonadZero (guard)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

length2 :: forall a. Array a -> Int
length2 arr =
  if null arr
    then 0
    else 1 + length2 (unsafePartial tail arr)

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
pairs' :: Int -> Array (Array Int)
pairs' n = concatMap (\i -> map(\j -> [i, j]) (1 .. n)) (1 .. n)
pairs'' :: Int -> Array (Array Int)
pairs'' n = concatMap (\i -> map(\j -> [i, j]) (i .. n)) (1 .. n)
factors :: Int -> Array (Array Int)
factors n = filter (\pair -> product pair == n) (pairs'' n)

factors' :: Int -> Array (Array Int)
factors' n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  --pure [i, j]
  [[i, j]]

factors'' :: Int -> Array (Array Int)
factors'' n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = length (factors'' n) == 1

cprod :: Array Int -> Array Int -> Array (Array Int)
cprod a b = do
  i <- a
  j <- b
  pure [i,j]

pytTriple :: Int -> Array (Array Int)
pytTriple n = do
  a <- 1 .. n
  b <- 1 .. n
  c <- 1 .. n
  guard $ a + b + c < 100
  guard $ a*a + b*b == c*c
  pure [a, b, c]

-- could not figure out a solution
{-
factorizations :: Int -> Array (Array Int)
factorizations n = do
  let x = factors'' n
  a <- x
  if isPrime a[0]
    then a
    else a
-}

factTr :: Int -> Int -> Int
factTr 0 acc = acc
factTr n acc = factTr (n - 1) (acc * n)

reverse1 :: forall a. Array a -> Array a
reverse1 [] = []
reverse1 xs = snoc (reverse1 (unsafePartial tail xs))
                  (unsafePartial head xs)

reverse2 :: forall a. Array a -> Array a
reverse2 = reverse2' []
  where
    reverse2' acc [] = acc
    reverse2' acc xs = reverse2' (cons (unsafePartial head xs) acc)
                               (unsafePartial tail xs)

count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = if p (unsafePartial head xs)
  then count p (unsafePartial tail xs) + 1
  else count p (unsafePartial tail xs)

countTr :: forall a. (a -> Boolean) -> Array a -> Int
countTr = countTr' 0
  where
    countTr' acc _ [] = acc
    countTr' acc p xs = if p (unsafePartial head xs)
      then countTr' (acc + 1) p (unsafePartial tail xs)
      else countTr' acc p (unsafePartial tail xs)

reverseFr :: forall a. Array a -> Array a
reverseFr = foldr (\x xs -> xs <> [x]) []


reverseFl :: forall a. Array a -> Array a
reverseFl = foldl (\xs x -> [x] <> xs) []

main :: Effect Unit
main = do
  log (show (fact 2))
  log (show (fib 2))
  log (show (fib 3))
  log (show (fib 4))
  log (show (length2 [1,2,4]))
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
  log (">> filter start")
  log (show (filter (\n -> n > -1) (-10 .. 10)))
  log (show ((\n -> n > -1) `filter` (-10 .. 10)))
  log (show ((\n -> n > -1) <$?> (-10 .. 10)))
  log (show ((<$?>) (\n -> n > -1)  (-10 .. 10)))
  log (show ((\n -> n > -1) <$?> (-10 .. 10)))
  log (">> flatten start")
  log (show (concat [[1,2],[4,5,6], [7]]))
  log (show (concatMap (\n -> [n, n*n]) (1 .. 5)))
  log (">> arr comprehensions")
  log (show (pairs 3))
  log (show (pairs' 3))
  log (show (pairs'' 3))
  log (show (factors 3))
  log (show (factors 10))
  log (show (factors' 10))
  log (show (factors'' 10))
  log (show (length [[1,2],[4,5,6], [7]]))
  log (show (isPrime 10))
  log (show (isPrime 19))
  log (show (cprod [11,22,33] [4,5,6]))
  log (show (pytTriple 100))
  --log (show (factorizations 8)) -- could not figure out a solution
  log (">> folds")
  log (show (foldl (\a n -> a <> show n) "" [3,4,5]))
  log (show (foldr (\n a -> a <> show n) "" [3,4,5]))
  log (">> tail recursion & accumulators")
  log (show (factTr 5 1))
  log (show (reverse1 [1, 2, 3]))
  log (show (reverse2 [1, 2, 3]))
  log (">> exercise foldl")
  log (show (foldl (\a n -> a && n) true [true, true, true]))
  log (show (foldl (\a n -> a && n) true [true, false, true]))
  log (">> exercise characterize fold")
  log (show (foldl (==) false [true, false, true]))
  log (show (foldl (==) false [true, false, false]))
  log (show (foldl (==) false [false, false, false]))
  log (">> exercise tail recursive conversion")
  log (show (count (\a -> a == false) [false, true, false]))
  log (show (countTr (\a -> a == false) [false, true, false]))
  log (show (countTr (\a -> a == false) [false, false, false]))
  log (">> reverse fold ")
  log (show (reverseFr [1, 2, 3]))
  log (show (reverseFl [1, 2, 3]))
