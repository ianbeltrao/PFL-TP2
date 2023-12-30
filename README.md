
# Haskell Project README

## Project Overview
This Haskell project is an implementation of a stack-based language interpreter. The main functionalities include parsing, compiling, and executing a simple programming language with arithmetic and boolean expressions, conditional statements, and looping constructs.

## Group Formation
- **Group Number:** T09 G03
- **Members:**
  - Lucas Borborema Nakajima: 50%
  - Ian da Nóbrega Beltrão: 50%

## Implementation Details
- **Parser and Lexer:** The parser and lexer are responsible for converting input strings into a series of instructions. These components handle various expressions and statements, including arithmetic operations, variable assignments, and control structures.
- **Arithmetic Operations:** The implementation supports basic arithmetic operations such as addition, subtraction, and multiplication.
- **Control Structures:** Conditional statements (if-else) and looping constructs (while) are implemented to control the flow of the program.
- **Stack Manipulation:** The core of the program operates on a stack, where all computations are performed.
- **Error Handling:** The program includes error handling for invalid operations and syntax errors.

## Usage Instructions
1. Ensure that GHC (Glasgow Haskell Compiler) is installed on your system.
2. Navigate to the directory containing `main.hs`.
3. Compile the program using GHC:
   ```
   ghc main.hs
   ```
4. Run the compiled program:
   ```
   .\main.exe
   ```
