-- | Virtual machine for the Synacor Challenge
module Machine (runInteractive, trace, Instruction, Action) where

import State
import Data.Bits

-- | Machine instructions
data Instruction
    = Halt
    | Set Arg Arg
    | Push Arg
    | Pop Arg
    | Eq Arg Arg Arg
    | Gt Arg Arg Arg
    | Jmp Arg
    | JT Arg Arg
    | JF Arg Arg
    | Add Arg Arg Arg
    | Mult Arg Arg Arg
    | Mod Arg Arg Arg
    | And Arg Arg Arg
    | Or Arg Arg Arg
    | Not Arg Arg
    | RMem Arg Arg
    | WMem Arg Arg
    | Call Arg
    | Ret
    | Out Arg
    | In Arg
    | NoOp
    deriving Show

-- | Instruction argument
data Arg = Constant Int | Register Int
    deriving Show

-- | Fetch the current instruction, advancing the PC past it.
fetch :: State -> (Instruction, State)
fetch = runFetch $ do
    opcode <- word
    ops!!opcode

ops :: [Fetch Instruction]
ops = [
    pure Halt,
    Set <$> arg <*> arg,
    Push <$> arg,
    Pop <$> arg,
    Eq <$> arg <*> arg <*> arg,
    Gt <$> arg <*> arg <*> arg,
    Jmp <$> arg,
    JT <$> arg <*> arg,
    JF <$> arg <*> arg,
    Add <$> arg <*> arg <*> arg,
    Mult <$> arg <*> arg <*> arg,
    Mod <$> arg <*> arg <*> arg,
    And <$> arg <*> arg <*> arg,
    Or <$> arg <*> arg <*> arg,
    Not <$> arg <*> arg,
    RMem <$> arg <*> arg,
    WMem <$> arg <*> arg,
    Call <$> arg,
    pure Ret,
    Out <$> arg,
    In <$> arg,
    pure NoOp]

arg :: Fetch Arg
arg = fmap toArg word

-- Decoding instruction arguments:
-- numbers 0..32767 mean a literal value
-- numbers 32768..32775 instead mean registers 0..7
-- numbers 32776..65535 are invalid
toArg :: Int -> Arg
toArg n
  | n < 32768 = Constant n
  | r <= 7 = Register r
  | otherwise = error "invalid value"
  where
    r = n - 32768

-- | Instruction actions
data Action
    = Finish
    | DoNothing
    | SetLoc Arg Int
    | Write Char
    | ReadTo Arg
    | JumpTo Int
    | PopTo Arg
    | PushVal Int
    | PushJ Int
    | PopJ
    deriving Show

-- | The action of an instruction
execute :: State -> Instruction -> Action
-- stop execution and terminate the program
execute _ Halt = Finish
-- set register <a> to the value of <b>
execute s (Set a b) = SetLoc a (getArg b s)
-- push <a> onto the stack
execute s (Push a) = PushVal (getArg a s)
-- remove the top element from the stack and write it into <a>; empty
-- stack = error
execute _ (Pop a) = PopTo a
-- set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
execute s (Eq a b c) = SetLoc a (fromEnum (getArg b s == getArg c s))
-- set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
execute s (Gt a b c) = SetLoc a (fromEnum (getArg b s > getArg c s))
-- jump to <a>
execute s (Jmp a) = JumpTo (getArg a s)
-- if <a> is nonzero, jump to <b>
execute s (JT a b)
  | getArg a s /= 0 = JumpTo (getArg b s)
  | otherwise = DoNothing
-- if <a> is zero, jump to <b>
execute s (JF a b)
  | getArg a s == 0 = JumpTo (getArg b s)
  | otherwise = DoNothing
-- assign into <a> the sum of <b> and <c> (modulo 32768)
execute s (Add a b c) = SetLoc a ((getArg b s + getArg c s) `mod` 32768)
-- store into <a> the product of <b> and <c> (modulo 32768)
execute s (Mult a b c) = SetLoc a ((getArg b s * getArg c s) `mod` 32768)
-- store into <a> the remainder of <b> divided by <c>
execute s (Mod a b c) = SetLoc a (getArg b s `mod` getArg c s)
-- stores into <a> the bitwise and of <b> and <c>
execute s (And a b c) = SetLoc a (getArg b s .&. getArg c s)
-- stores into <a> the bitwise or of <b> and <c>
execute s (Or a b c) = SetLoc a (getArg b s .|. getArg c s)
-- stores 15-bit bitwise inverse of <b> in <a>
execute s (Not a b) = SetLoc a (getArg b s `xor` 32767)
-- read memory at address <b> and write it to <a>
execute s (RMem a b) = SetLoc a (getMemory (getArg b s) s)
-- write the value from <b> into memory at address <a>
execute s (WMem a b) = SetLoc (Constant (getArg a s)) (getArg b s)
-- write the address of the next instruction to the stack and jump to <a>
execute s (Call a) = PushJ (getArg a s)
-- remove the top element from the stack and jump to it; empty stack
-- = halt
execute _ Ret = PopJ
-- write the character represented by ascii code <a> to the terminal
execute s (Out a) = Write (toEnum (getArg a s))
-- read a character from the terminal and write its ascii code to <a>;
-- it can be assumed that once input starts, it will continue until a
-- newline is encountered; this means that you can safely read whole
-- lines from the keyboard and trust that they will be fully read
execute _ (In a) = ReadTo a
-- no operation
execute _ NoOp = DoNothing

-- An argument used as a source of an instruction yields either a constant
-- or the contents of a register.
getArg :: Arg -> State -> Int
getArg (Constant n) _ = n
getArg (Register r) s = getRegister r s

-- | Apply an action to a state and input string.
-- We don't collect output, as that can be retrieved from the action.
apply :: Action -> State -> String -> Maybe (State, String)
apply Finish _ _ = Nothing
apply DoNothing s cs = Just (s, cs)
apply (SetLoc n v) s cs = Just (setArg n v s, cs)
apply (Write _) s cs = Just (s, cs)
apply (ReadTo _) _ [] = Nothing
apply (ReadTo a) s (c:cs) = Just (setArg a (fromEnum c) s, cs)
apply (JumpTo n) s cs = Just (setPC n s, cs)
apply (PopTo a) s cs = Just (setArg a v s', cs)
  where
    (s', v) = popStack s
apply (PushVal v) s cs = Just (pushStack v s, cs)
apply (PushJ n) s cs = Just (setPC n (pushStack (getPC s) s), cs)
apply PopJ s cs = Just (setPC n s', cs)
  where
    (s', n) = popStack s

-- An argument used as the target of an instruction indicates setting of
-- either a memory location or a register.
setArg :: Arg -> Int -> State -> State
setArg (Constant n) v s = setMemory n v s
setArg (Register r) v s = setRegister r v s

-- Running the machine

-- | Run a state as a lazy string stransformer.
-- When a '*' character is input, apply the patch to the state.
runInteractive :: (State -> State) -> State -> String -> String
runInteractive patch s cs = [c | (_, _, Write c) <- trace patch s cs]

-- | Run a state with given input, yielding a full trace of actions.
-- When a '*' character is input, apply the patch to the state.
trace :: (State -> State) -> State -> String -> [(Int, Instruction, Action)]
trace patch = run
  where
    run s ('*':cs) = run (patch s) cs
    run s cs = (getPC s, op, action):case apply action s1 cs of
        Nothing -> []
        Just (s', cs') -> run s' cs'
      where
        (op, s1) = fetch s
        action = execute s1 op
