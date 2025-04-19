{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- | Infinite stream of elements
data Stream a = Stream a (Stream a)

-- | Show instance for Stream, showing the first 10 elements
instance Show a => Show (Stream a) where
  show s = "[" ++ unwords (map show (take 10 (toList s))) ++ "]"

instance Functor Stream where
  fmap f (Stream x xs) = Stream (f x) (fmap f xs)

instance Foldable Stream where
  foldMap f (Stream x xs) = f x <> foldMap f xs

-- | Converts given list into stream
--
-- If the list is finite then it is continued
-- with given value repeated infinitely
--
-- Usage example:
--
-- >>> fromList 0 [1,2,3]
-- [1,2,3,0,0,0,0,0,0,0]
-- >>> fromList undefined [1..]
-- [1,2,3,4,5,6,7,8,9,10]
--
fromList :: a -> [a] -> Stream a
fromList def [] = Stream def (fromList def [])
fromList def (x:xs) = Stream x (fromList def xs)

-- | Builds stream from given seed value by applying given step function
--
-- Step function produces a pair of the next element in stream and updated seed value.
--
-- Usage example:
--
-- >>> unfold (\x -> (x, x-1)) 5
-- [5,4,3,2,1,0,-1,-2,-3,-4]
-- >>> unfold (\x -> (abs x, x-1)) 5
-- [5,4,3,2,1,0,1,2,3,4]
--
unfold :: (b -> (a, b)) -> b -> Stream a
unfold f seed = Stream a (unfold f b)
  where (a, b) = f seed

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = unfold (\n -> (n, n + 1)) 1

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = unfold (\(a, b) -> (a, (b, a + b))) (0, 1)

-- | Helper function to filter Stream based on predicate
filterStream :: (a -> Bool) -> Stream a -> Stream a
filterStream p (Stream x xs)
  | p x = Stream x (filterStream p xs)
  | otherwise = filterStream p xs

-- | Returns infinite stream of prime numbers
--
-- First 10 prime numbers:
--
-- >>> primes
-- [2,3,5,7,11,13,17,19,23,29]
--
primes :: Stream Integer
primes = Stream 2 (filterStream isPrime (Stream 3 (unfold (\n -> (n, n + 2)) 5)))
  where
    isPrime n = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) (toList primes))

-- | Helper function to take elements from Stream
take' :: Int -> Stream a -> [a]
take' 0 _ = []
take' n (Stream x xs) = x : take' (n-1) xs

-- | Helper function to convert Stream to list
toList :: Stream a -> [a]
toList (Stream x xs) = x : toList xs

-- | One step of Sieve of Eratosthenes
-- (to be used with 'unfoldr')
--
-- Returns next prime number from given stream
-- and strikes out all multiples of this prime
-- from the rest of the stream
--
-- Usage example:
--
-- >>> sieve $ fromList 0 [2..]
-- (2,[3,5,7,9,11,13,15,17,19,21])
-- >>> sieve $ snd $ sieve $ fromList 0 [2..]
-- (3,[5,7,11,13,17,19,23,25,29,31])
--
sieve :: Stream Integer -> (Integer, Stream Integer)
sieve (Stream p xs) = (p, filterStream (\x -> x `mod` p /= 0) xs)
