-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Data.Char (isSpace, isDigit, isLower, isAlpha)

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
arithmeticOp op (Left x:Left y:xs) = Left (op y x) : xs  -- Swapped x and y
arithmeticOp _ _ = error "Run-time error: Invalid stack for arithmetic operation"


-- Helper function for boolean comparison operations
boolOp :: (Integer -> Integer -> Bool) -> Stack -> Stack
boolOp op (Left x:Left y:xs) = Right (op x y):xs
boolOp _ _ = error "Run-time error"


loop :: Code -> Code -> Stack -> Code
loop c1 c2 stack = 
    case stack of
        (Right b:xs) -> if b 
                        then c1 ++ [Loop c1 c2] 
                        else []
        _ -> error "Run-time error: Invalid stack state in Loop"



run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst:rest, stack, state) =
    trace ("run: " ++ show inst ++ ", stack: " ++ show stack ++ ", state: " ++ show state) $
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
            _ -> error "Run-time error: Stack underflow in Equ"
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
    Branch c1 c2 -> 
        case stack of
            (Right True : xs) -> run (c1 ++ rest, xs, state)
            (Right False : xs) -> run (c2 ++ rest, xs, state)
            _ -> error "Run-time error: Invalid condition in Branch"
    Loop c1 c2 -> trace ("Looping with c1: " ++ show c1 ++ ", c2: " ++ show c2) $  -- Add this line for logging
                 run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ rest, stack, state)


-- Updated pop function with proper error handling
pop :: Stack -> (StackItem, Stack)
pop [] = error "Run-time error: Empty stack"
pop (x:xs) = (x, xs)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_, stack, state) = run(code, createEmptyStack, createEmptyState)

-- PFL 2023/24 - Haskell practical assignment Part 2
-- Updated on 15/12/2023

-- Part 2



-- Define the types Aexp, Bexp, Stm, and Program
data Aexp = Const Integer | Var String | AddExp Aexp Aexp | SubExp Aexp Aexp | MultExp Aexp Aexp
          deriving Show

data Bexp = TrueConst | FalseConst | EqualExp Aexp Aexp | LessEqExp Aexp Aexp | NotExp Bexp | AndExp Bexp Bexp
          deriving Show
data Stm = Assign String Aexp | Sequence [Stm] | If Bexp Stm Stm | While Bexp Stm
         deriving Show

-- compA :: Aexp -> Code
compA :: Aexp -> Code
compA a = {- trace ("compA: " ++ show a) $ -} case a of
    Const n -> [Push n]
    Var x -> [Fetch x]
    AddExp a1 a2 -> compA a1 ++ compA a2 ++ [Add]
    SubExp a1 a2 -> compA a1 ++ compA a2 ++ [Sub]
    MultExp a1 a2 -> compA a1 ++ compA a2 ++ [Mult]

-- compB :: Bexp -> Code
compB :: Bexp -> Code
compB b = {- trace ("compB: " ++ show b) $ -} case b of
    TrueConst -> [Tru]
    FalseConst -> [Fals]
    EqualExp a1 a2 -> compA a1 ++ compA a2 ++ [Equ]
    LessEqExp a1 a2 -> compA a1 ++ compA a2 ++ [Le]
    NotExp b -> compB b ++ [Neg]
    AndExp b1 b2 -> compB b1 ++ compB b2 ++ [And]

-- compile :: Program -> Code
compile :: [Stm] -> Code
compile stms = {- trace ("compile: " ++ show stms) $ -} case stms of
    [] -> []
    (s:rest) -> compileStm s ++ compile rest
  where
    compileStm s = {- trace ("compileStm: " ++ show s) $ -} case s of
        Assign x a -> compA a ++ [Store x]
        Sequence stmList -> concatMap compileStm stmList
        If b t f -> compB b ++ [Branch (compile [t]) (compile [f])]
        While b body -> let loopBody = compile [body] ++ compB b
                        in [Loop loopBody [Noop]]

-- Lexer and Parser Implementation
lexer :: String -> [String]
lexer str = {- trace ("lexer: " ++ str) $ -}
    case str of
        [] -> []
        (c:cs)
            | isSpace c -> lexer cs
            | isDigit c -> let (numToken, restNum) = span isDigit (c:cs) in numToken : lexer restNum
            | isAlpha c -> let (varToken, restVar) = span isAlpha (c:cs) in varToken : lexer restVar
            | otherwise -> [c] : lexer cs

parse :: String -> [Stm]
parse str = {- trace ("parse: " ++ str) $ -} parseStms (lexer str)

-- Parser for Arithmetic Expressions
parseAexp :: [String] -> (Aexp, [String])
parseAexp tokens = trace ("parseAexp: " ++ show tokens) $
    let (term, rest) = parseTerm tokens
    in case rest of
        ("+":rest') -> let (aexp, rest'') = parseAexp rest' in (AddExp term aexp, rest'')
        ("-":rest') -> let (aexp, rest'') = parseAexp rest' in (SubExp term aexp, rest'')
        _ -> (term, rest)

-- Parser for Terms in Arithmetic Expressions
parseTerm :: [String] -> (Aexp, [String])
parseTerm tokens = trace ("parseTerm: " ++ show tokens) $
    let (factor, rest) = parseFactor tokens
    in case rest of
        ("*":rest') -> let (term, rest'') = parseTerm rest' in (MultExp factor term, rest'')
        _ -> (factor, rest)

-- Parser for Factors in Arithmetic Expressions
parseFactor :: [String] -> (Aexp, [String])
parseFactor tokens = trace ("parseFactor: " ++ show tokens) $
    case tokens of
        "(" : rest -> 
            let (aexp, rest') = parseAexp rest
            in case span (/= ")") rest' of
                (_, ")" : rest'') -> (aexp, rest'')
                _ -> error "Parse error: Expected ')' after expression"
        t : rest
            | all isDigit t -> (Const (read t), rest)
            | otherwise -> (Var t, rest)


-- Parser for Boolean Expressions
parseBexp :: [String] -> (Bexp, [String])
parseBexp tokens = trace ("parseBexp: " ++ show tokens) $
    case tokens of
        ("not":rest) -> let (bexp, rest') = parseBexp rest in (NotExp bexp, rest')
        _ -> let (relation, rest) = parseRelation tokens
             in case rest of
                 ("and":rest') -> let (bexp, rest'') = parseBexp rest' in (AndExp relation bexp, rest'')
                 _ -> (relation, rest)

-- Parser for Relations in Boolean Expressions
parseRelation :: [String] -> (Bexp, [String])
parseRelation tokens = trace ("parseRelation: " ++ show tokens) $
    let (a1, op:rest) = parseAexp tokens
        (a2, rest') = parseAexp rest
    in case op of
        "<=" -> (LessEqExp a1 a2, rest')
        "==" -> (EqualExp a1 a2, rest')
        "=" -> let (b1, rest'') = parseBexp rest'
                   b1Val = case b1 of TrueConst -> 1; _ -> 0
               in (EqualExp (Const b1Val) a2, rest'')
        _ -> error "Parse error in parseRelation"

-- Parser for Statements
parseStm :: [String] -> (Stm, [String])
parseStm tokens = trace ("parseStm: " ++ show tokens) $
    case tokens of
        ("if":rest) -> 
            let (bexp, "then":restThen) = parseBexp rest
                (thenStm, "else":restElse) = parseStm restThen
                (elseStm, rest'') = parseStm restElse
            in (If bexp thenStm elseStm, rest'')
        
        ("while":rest) -> 
            let (bexp, "do":restDo) = parseBexp rest
                (body, rest') = parseStm restDo
            in (While bexp body, rest')

        (var:":=":rest) ->
            let (aexp, ";":rest') = parseAexp rest
            in (Assign var aexp, rest')

        _ -> 
            let (stm, rest') = parseSimpleStm tokens
            in case rest' of
                ";":rest'' -> let (stm', rest''') = parseStm rest'' in (Sequence [stm, stm'], rest''')
                _ -> (stm, rest')

-- Parser for Simple Statements (no control structures)
parseSimpleStm :: [String] -> (Stm, [String])
parseSimpleStm tokens = trace ("parseSimpleStm: " ++ show tokens) $
    case tokens of
        var:":":"=":rest -> 
            let (aexp, rest') = parseAexp rest
            in case rest' of
                ";":rest'' -> (Assign var aexp, rest'')
                _ -> error "Parse error: Expected ';' after assignment"
        _ -> error "Parse error: Expected assignment statement"

-- Parsing a sequence of statements
parseStms :: [String] -> [Stm]
parseStms tokens = trace ("parseStms: " ++ show tokens) $
    case tokens of
        [] -> []
        _ -> let (stm, rest) = parseStm tokens
             in stm : parseStms rest

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = 
    let result = (stack2Str stack, state2Str state)
    in trace ("testParser result: " ++ show result) result
  where 
    (_, stack, state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


main :: IO ()
main = do
  --print $ testParser "x := 5; x := x - 1;" == ("","x=4")
  --print $ testParser "x := 0 - 2;" == ("","x=-2")
  print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
  print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
  print $ testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
  print $ testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
  print $ testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
  print $ testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
  print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
  print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")


-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")