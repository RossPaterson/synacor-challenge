-- The orb grid puzzle from the Synacore Challenge
module Main where

import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map

grid :: [[String]]
grid = [
    ["*",  "8",  "-",  "1"],
    ["4",  "*", "11",  "*"],
    ["+",  "4",  "-", "18"],
    ["22", "-",  "9",  "*"]]

-- We need to find a path from the start (bottom left) to the finish (top
-- right) that evaluates to 30.  We cannot revisit either the start of
-- the finish, but we can revisit other rooms, and we need not visit all
-- rooms.  Since there is a time limit, we seek the shortest such path.

type Point = (Int, Int)

gridMap :: Map Point String
gridMap = Map.fromList
    [((x, y), s) | (y, row) <- zip [0..] grid, (x, s) <- zip [0..] row]

start, finish :: Point
start = (0, 3)
finish = (3, 0)

target :: Int
target = 30

data Direction = North | South | East | West
    deriving Show

directions :: [Direction]
directions = [North, South, East, West]

move :: Direction -> Point -> Point
move North (x, y) = (x, y-1)
move South (x, y) = (x, y+1)
move East (x, y) = (x+1, y)
move West (x, y) = (x-1, y)

-- possible paths (reversed), in breadth-first order
paths :: Map Point a -> Point -> [[[Point]]]
paths m s = iterate explore [[s]]
  where
    explore pss = [p':ps |
        ps <- pss,
        let p = head ps,
        p /= finish,
        d <- directions,
        let p' = move d p,
        p' /= start,
        Map.member p' m]

evalPath :: [String] -> Maybe Int
evalPath [] = error "empty expression"
evalPath (s:mods) = evalPath' (read s) mods
  where
    evalPath' :: Int -> [String] -> Maybe Int
    evalPath' x _ | x <= 0 = Nothing
    evalPath' x [] = Just x
    evalPath' x ("+":y:rest) = evalPath' (x + read y) rest
    evalPath' x ("-":y:rest) = evalPath' (x - read y) rest
    evalPath' x ("*":y:rest) = evalPath' (x * read y) rest
    evalPath' _ _ = error "bad expression"

solve :: Map Point String -> [Direction]
solve m = dirs $ head [ps |
    rev_ps <- concat (paths m start),
    head rev_ps == finish,
    let ps = reverse rev_ps,
    let path = map (m!) ps,
    evalPath path == Just target]

-- reconstruct directions taken from a path
dirs :: [Point] -> [Direction]
dirs ps = zipWith dir ps (tail ps)
  where
    dir p1 p2 = head [d | d <- directions, move d p1 == p2]

main :: IO ()
main = putStrLn $ map toLower $ unwords $ map show $ solve gridMap
