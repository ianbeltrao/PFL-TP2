-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

import Data.List (intercalate, sortBy)
import Data.Ord (comparing)

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- Type Definitions
type StackItem = Either Integer Bool  -- Either an Integer or a Bool
type Stack = [StackItem]
type State = [(String, StackItem)]

-- createEmptyStack :: Stack
createEmptyStack :: Stack
createEmptyStack = []

-- stack2Str :: Stack -> String
stackItemToStr :: StackItem -> String
stackItemToStr (Left n) = show n
stackItemToStr (Right b) = if b then "True" else "False"

stack2Str :: Stack -> String
stack2Str = intercalate "," . map stackItemToStr

-- createEmptyState :: State
createEmptyState :: State
createEmptyState = []

-- state2Str :: State -> String
state2Str :: State -> String
state2Str state = intercalate "," $ map (\(var, val) -> var ++ "=" ++ stackItemToStr val) $ sortBy (comparing fst) state

-- Helper function to check if a StackItem is a Bool
isBool :: StackItem -> Bool
isBool (Right _) = True
isBool _ = False

-- Helper function to check if a StackItem is an Integer
isInteger :: StackItem -> Bool
isInteger (Left _) = True
isInteger _ = False

-- Helper function for arithmetic operations
arithmeticOp :: (Integer -> Integer -> Integer) -> Stack -> Stack
arithmeticOp op (Left x:Left y:xs) = Left (op y x):xs
arithmeticOp _ _ = error "Run-time error"

-- Helper function for boolean comparison operations
boolOp :: (Integer -> Integer -> Bool) -> Stack -> Stack
boolOp op (Left x:Left y:xs) = Right (op y x):xs
boolOp _ _ = error "Run-time error"

loop :: Code -> Code -> Stack -> Code
loop c1 c2 stack = case pop stack of
                     (Right True, xs) -> c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] 
                     (Right False, xs) -> []
                     _ -> error "Run-time error"


run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((inst:rest), stack, state) =
  case inst of
    Push n -> run (rest, Left n : stack, state)
    Add    -> run (rest, arithmeticOp (+) stack, state)
    Mult   -> run (rest, arithmeticOp (*) stack, state)
    Sub    -> run (rest, arithmeticOp (-) stack, state)
    Tru    -> run (rest, Right True : stack, state)
    Fals   -> run (rest, Right False : stack, state)
    Equ    -> case stack of
                (Left x:Left y:xs)  -> run (rest, Right (x == y) : xs, state)
                (Right x:Right y:xs) -> run (rest, Right (x == y) : xs, state)
                _ -> error "Run-time error"
    Le     -> run (rest, boolOp (<=) stack, state)
    And    -> case pop stack of
                (Right x, Right y:xs) -> run (rest, Right (x && y) : xs, state)
                _ -> error "Run-time error"
    Neg    -> case pop stack of
                (Right x, xs) -> run (rest, Right (not x) : xs, state)
                _ -> error "Run-time error"
    Fetch var -> case lookup var state of
                    Just val -> run (rest, val : stack, state)
                    Nothing  -> error "Run-time error"
    Store var -> case pop stack of
                    (val, xs) -> run (rest, xs, (var, val) : filter ((/= var) . fst) state)
    Noop -> run (rest, stack, state)
    Branch c1 c2 -> case pop stack of
                    (Right True, xs) -> run (c1 ++ rest, xs, state)
                    (Right False, xs) -> run (c2 ++ rest, xs, state)
                    _ -> error "Run-time error"
    Loop c1 c2 -> run (loop c1 c2 stack ++ rest, stack, state)    

pop :: Stack -> (StackItem, Stack)
pop (x:xs) = (x, xs)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_, stack, state) = run(code, createEmptyStack, createEmptyState)

main :: IO ()
main = do
  -- Example test case
  print $ testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
  print $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
  print $ testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
  print $ testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
  print $ testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
  print $ testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
  print $ testAssembler [Push (-20),Push (-21), Le] == ("True","")
  print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
  print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- Examples:
-- -- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- -- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- -- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- ?? testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"
