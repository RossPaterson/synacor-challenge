Solving the teleporter puzzle of the Synacor Challenge.

> module Main where

> import Data.Array

The crucial code of the teleporter confirmation mechanism is:

    5483: set r0 4
    5486: set r1 1
    5489: call 6027
    5491: eq r1 r0 6
    5495: jf r1 5579
    5498: ...

That is, r7 needs to be set to a value such that the function at 6027,
called with r0=4 and r1=1, returns with r0=6.  (The final value of r1
is unused.)  The function at 6027 is

    6027: jt r0 6035
    6030: add r0 r1 1
    6034: ret
    6035: jt r1 6048
    6038: add r0 r0 32767
    6042: set r1 r7
    6045: call 6027
    6047: ret
    6048: push r0
    6050: add r1 r1 32767
    6054: call 6027
    6056: set r1 r0
    6059: pop r0
    6061: add r0 r0 32767
    6065: call 6027
    6067: ret

Re-expressed in Haskell (with x, m, n for r7, r0, r1 respectively), this is

> hash :: Integer -> Integer -> Integer -> Integer
> hash x m n
>   | m == 0 = (n + 1) `mod` 32768
>   | n == 0 = hash x (m-1) x
>   | otherwise = hash x (m-1) (hash x m (n-1))

The special case where x = 1 is the usual Ackermann function (modulo 32768).

We are required to find x such that

    hash x 4 1 == 6

There seems no alternative to exhaustive search, calling hash for each
possible x:

> solutions :: (Integer -> Integer -> Integer -> Integer) -> [Integer]
> solutions h = [x | x <- [1..32767], h x 4 1 == 6]

However, this implementation is far too slow.  There are two approaches
to speeding it up:

(1) Algebraic transformation

By induction on the last two clauses, we obtain:

    hash x (m+1) n = times (n+1) (hash x m) x

for the iteration function

> times :: Integer -> (a -> a) -> a -> a
> times n f x
>   | n == 0 = x
>   | otherwise = f (times (n-1) f x)

This becomes the base case of the re-implementation.
A bit of algebra yields faster equivalents for m = 1..3.

> hashA :: Integer -> Integer -> Integer -> Integer
> hashA x m n
>   | m == 0 = (n + 1) `mod` 32768
>   | m == 1 = (n + x + 1) `mod` 32768
>   | m == 2 = ((x+1)*n + 2*x + 1) `mod` 32768
>   -- hashA x 3 n = (x+1)^(n+2) + ... + (x+1)^2 + (x+1) - 1
>   | m == 3 = (((x+1)^(n+3) - 1) `div` x - 2) `mod` 32768
>   | otherwise = times (n+1) (hashA x (m-1)) x

(2) Dynamic programming

Because the arguments are limited to 15 bits, it is feasible to avoid
repeated recursive calls by tabulating the hash function for each x.

First, we parameterize out the recursion, and use Ints for compactness:

> hashShape :: (Int -> Int -> Int) -> Int -> Int -> Int -> Int
> hashShape h x m n
>   | m == 0 = (n + 1) `mod` 32768
>   | n == 0 = h (m-1) x
>   | otherwise = h (m-1) (h m (n-1))

Then for each x we use a list of arrays to cache hash x m n:

> hashD :: Integer -> Integer -> Integer -> Integer
> hashD x a b = toInteger (h (fromInteger a) (fromInteger b))
>   where
>     x' = fromInteger x
>     h m n = cache!!m!n
>     cache :: [Array Int Int]
>     cache = [listArray (0,bound) [hashShape h x' m n | n <- [0..bound]] | m <- [0..]]
>     bound = 32767

> main :: IO ()
> main = putStr $ unlines $ map show (solutions hashA)
