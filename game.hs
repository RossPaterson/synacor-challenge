-- Adventure-style game of the Synacor Challenge
module Main where

import State
import Machine

main :: IO ()
main = do
    s <- readState "challenge.bin"
    script <- readFile "script"
    let commands = unlines [line | line <- lines script, head line /= '#']
    putStr $ runInteractive teleporterPatch s commands
    -- putStr $ unlines $ map show $ trace teleporterPatch s commands

-- Patch to modify the teleporter take us to the rest of the game
-- (see teleporter.lhs for the various values)
-- This has to be done after the initial self-test but before using
-- the teleporter.
teleporterPatch :: State -> State
teleporterPatch s =
    -- set r7 to make the teleporter go to the other destination
    -- (value obtained by inverting the confirmation function)
    setRegister 7 25734 $
    -- short-circuit teleporter confirmation function:
    --     set r0 6 (output value that is tested for in later code)
    --     ret
    foldr (uncurry setMemory) s (zip [6027..] [1, 32768, 6, 18])
