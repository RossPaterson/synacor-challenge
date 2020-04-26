Solving the teleporter puzzle of the Synacor Challenge.

> module Main where

The crucial code of the teleporter confirmation mechanism is:

    5483: set r0 4
    5486: set r1 1
    5489: call 6027
    5491: eq r1 r0 6
    5495: jf r1 5579
    5498: ...

That is, the function at 6027, called with r0=4 and r1=1, should return
with r0=6.  (The final value of r1 is unused.)  The function at 6027 is

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

However, this implementation is far too slow.

By induction on the last two clauses, we obtain:

    hash x (m+1) n = times (n+1) (hash x m) x

for the iteration function

> times :: Integer -> (a -> a) -> a -> a
> times n f x
>   | n == 0 = x
>   | otherwise = f (times (n-1) f x)

This becomes the base case of the re-implementation.
A bit of algebra yields faster equivalents for m = 1..3.

> hash' :: Integer -> Integer -> Integer -> Integer
> hash' x m n
>   | m == 0 = (n + 1) `mod` 32768
>   | m == 1 = (n + x + 1) `mod` 32768
>   | m == 2 = ((x+1)*n + 2*x + 1) `mod` 32768
>   -- hash' x 3 n = (x+1)^(n+2) + ... + (x+1)^2 + (x+1) - 1
>   | m == 3 = (((x+1)^(n+3) - 1) `div` x - 2) `mod` 32768
>   | otherwise = times (n+1) (hash' x (m-1)) x

> solutions :: [Integer]
> solutions = [x | x <- [1..32767], hash' x 4 1 == 6]

> main :: IO ()
> main = putStr $ unlines $ map show solutions
