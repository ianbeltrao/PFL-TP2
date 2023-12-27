-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- Stack and State Element Types
data StackElement = IntVal Integer | BoolVal Bool deriving (Eq, Show)
type Stack = [StackElement]
type State = [(String, StackElement)]

-- Create an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Create an empty state
createEmptyState :: State
createEmptyState = []

-- Convert stack to string
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (x:xs) = stackElement2Str x ++ (if not (null xs) then "," ++ stack2Str xs else "")
  where
    stackElement2Str (IntVal n) = show n
    stackElement2Str (BoolVal b) = if b then "True" else "False"

-- Convert state to string
state2Str :: State -> String
state2Str = concatMap (\(var, val) -> var ++ "=" ++ stackElement2Str val ++ ",") . sortOn fst
  where
    stackElement2Str (IntVal n) = show n
    stackElement2Str (BoolVal b) = if b then "True" else "False"

-- Run function
run :: (Code, Stack, State) -> (Code, Stack, State)
run (code, stack, state) = case code of
  [] -> ([], stack, state)
  (inst:rest) -> case inst of
    Push n -> run (rest, IntVal n : stack, state)
    Add -> runBinaryOp (+) rest stack state
    Mult -> runBinaryOp (*) rest stack state
    Sub -> runBinaryOp (-) rest stack state
    Tru -> run (rest, BoolVal True : stack, state)
    Fals -> run (rest, BoolVal False : stack, state)
    Equ -> runBinaryBoolOp (==) rest stack state
    Le -> runBinaryBoolOp (<=) rest stack state
    And -> runBinaryBoolOp (&&) rest stack state
    Neg -> runUnaryBoolOp not rest stack state
    Fetch var -> runFetch var rest stack state
    Store var -> runStore var rest stack state
    Noop -> run (rest, stack, state)
    Branch c1 c2 -> runBranch c1 c2 rest stack state
    Loop c1 c2 -> runLoop c1 c2 rest stack state
    _ -> error "Run-time error"

-- Helper functions for run
runBinaryOp :: (Integer -> Integer -> Integer) -> Code -> Stack -> State -> (Code, Stack, State)
runBinaryOp op (x:y:rest) state = case (x, y) of
  (IntVal a, IntVal b) -> run (rest, IntVal (op a b) : rest, state)
  _ -> error "Run-time error"

runBinaryBoolOp :: (Bool -> Bool -> Bool) -> Code -> Stack -> State -> (Code, Stack, State)
runBinaryBoolOp op (x:y:rest) state = case (x, y) of
  (BoolVal a, BoolVal b) -> run (rest, BoolVal (op a b) : rest, state)
  _ -> error "Run-time error"

runUnaryBoolOp :: (Bool -> Bool) -> Code -> Stack -> State -> (Code, Stack, State)
runUnaryBoolOp op (x:rest) state = case x of
  BoolVal a -> run (rest, BoolVal (op a) : rest, state)
  _ -> error "Run-time error"

runFetch :: String -> Code -> Stack -> State -> (Code, Stack, State)
runFetch var rest stack state = case lookup var state of
  Just val -> run (rest, val : stack, state)
  Nothing -> error "Run-time error"

runStore :: String -> Code -> Stack -> State -> (Code, Stack, State)
runStore var (x:rest) state = case x of
  val -> run (rest, stack, (var, val) : state)
  _ -> error "Run-time error"

runBranch :: Code -> Code -> Code -> Stack -> State -> (Code, Stack, State)
runBranch c1 c2 rest (x:xs) state = case x of
  BoolVal True -> run (c1 ++ rest, xs, state)
  BoolVal False -> run (c2 ++ rest, xs, state)
  _ -> error "Run-time error"

runLoop :: Code -> Code -> Code -> Stack -> State -> (Code, Stack, State)
runLoop c1 c2 rest stack state = run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) Noop] ++ rest, stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_, stack, state) = run (code, createEmptyStack, createEmptyState)
