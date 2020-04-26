-- The coin puzzle from the Synacore Challenge
module Main where

import Data.Char
import Data.List

-- coins found in the ruins
data Coin = Red | Corroded | Shiny | Concave | Blue
    deriving (Bounded, Enum, Show)

coins :: [Coin]
coins = [minBound..maxBound]

-- number on each coin
value :: Coin -> Int
value Red = 2
value Corroded = 3
value Shiny = 5
value Concave = 7
value Blue = 9

-- the formula on the monument in the massive central hall of the ruins
equation :: [Int] -> Bool
equation [a, b, c, d, e] = a + b * c^(2::Int) + d^(3::Int) - e == 399
equation _ = error "five coins expected"

solutions :: [[Coin]]
solutions = [cs | cs <- permutations coins, equation (map value cs)]

main :: IO ()
main = putStrLn $ map toLower $ unwords $ map show $ head solutions
