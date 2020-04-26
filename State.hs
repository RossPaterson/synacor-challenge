-- | State of the virtual machine for the Synacor Challenge
module State (
    State, readState,
    getMemory, setMemory, getRegister, setRegister,
    pushStack, popStack, getPC, setPC,
    Fetch, runFetch, word
    ) where

import Control.Monad
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

-- Read 16-bit numbers stored as 16-bit little-endian pairs

readInts :: FilePath -> IO [Int]
readInts name =
    withBinaryFile name ReadMode $ \ h -> do
        cs <- hGetContents h
        let ws = [fromEnum c2 * 256 + fromEnum c1 | (c1, c2) <- pairs cs]
        length ws `seq` return ws

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [_] = error "odd length list"
pairs (x1:x2:xs) = (x1, x2):pairs xs

-- | Machine state
data State = State {
    -- | memory with 15-bit address space storing 16-bit values
    memory :: Map Int Int,
    -- | eight registers
    register :: Map Int Int,
    -- | unbounded stack holding individual 16-bit values
    stack :: [Int],
    -- | location in memory of the next instruction to be executed
    pc :: Int
    }
    deriving Show

-- Load a program into memory starting at address 0
initState :: [Int] -> State
initState vs = State {
    memory = Map.fromList (zip [0..] vs),
    register = Map.empty,
    stack = [],
    pc = 0
    }

-- | Load a program from a file
readState :: FilePath -> IO State
readState name = do
    ns <- readInts name
    return (initState ns)

-- Basic state operations

getMemory :: Int -> State -> Int
getMemory n s = Map.findWithDefault (error msg) n (memory s)
  where
    msg = "bad address " ++ show n

setMemory :: Int -> Int -> State -> State
setMemory n v s
  | 0 <= n && n < 32768 = s { memory = Map.insert n v (memory s) }
  | otherwise = error $ "bad memory address " ++ show n

getRegister :: Int -> State -> Int
getRegister n s = Map.findWithDefault 0 n (register s)

setRegister :: Int -> Int -> State -> State
setRegister n v s = s { register = Map.insert n v (register s) }

pushStack :: Int -> State -> State
pushStack v s = s { stack = v:stack s }

popStack :: State -> (State, Int)
popStack s = case stack s of
    [] -> error "empty stack"
    v:vs -> (s { stack = vs }, v)

getPC :: State -> Int
getPC = pc

setPC :: Int -> State -> State
setPC loc s = s { pc = loc }

-- | Monad for fetching words at the current position
newtype Fetch a = Fetch (State -> (a, State))

runFetch :: Fetch a -> State -> (a, State)
runFetch (Fetch f) = f

-- | Fetch one 16-bit word
word :: Fetch Int
word = Fetch $ \ s -> let loc = getPC s in (getMemory loc s, setPC (loc+1) s)

instance Functor Fetch where
    fmap f (Fetch p) = Fetch $ \ s -> let (x, s') = p s in (f x, s')

instance Applicative Fetch where
    pure x = Fetch $ \ s -> (x, s)
    (<*>) = ap

instance Monad Fetch where
    Fetch px >>= k = Fetch $ \ s -> let (x, s') = px s in runFetch (k x) s'
